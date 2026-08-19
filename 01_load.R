library(tidyverse)


# csv ####
full_x_20260818 <- read_csv("zeeschuimer-export-twitter.com-2026-08-18T10_11_55.605Z.csv")

problems(full_x_20260818)
dim(full_x_20260818) # sth wrong, 27373 obs. - should be over 40K
# maybe using zeehaven was a mistake,
# let's parse source ndjson file 

# ndjson #### 

library(jsonlite)
library(purrr)

path <- "zeeschuimer-export-twitter.com-2026-08-18T100809.ndjson"
lines <- readLines(path, warn = FALSE)
lines <- lines[nzchar(lines)]

raw_items <- map(lines, ~ fromJSON(.x, simplifyVector = FALSE))

# ---- typed pluck helpers (avoid bind_rows type-mismatch errors) ----
safe_chr <- function(lst, ..., default = NA_character_) {
  val <- pluck(lst, ..., .default = NULL)
  if (is.null(val) || is.list(val)) return(default)
  as.character(val)
}
safe_num <- function(lst, ..., default = NA_real_) {
  val <- pluck(lst, ..., .default = NULL)
  if (is.null(val) || is.list(val)) return(default)
  suppressWarnings(as.numeric(val))
}
safe_lgl <- function(lst, ..., default = NA) {
  val <- pluck(lst, ..., .default = NULL)
  if (is.null(val) || is.list(val)) return(default)
  as.logical(val)
}

collapse_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- compact(x)
  if (length(x) == 0) return(default)
  paste(unlist(x), collapse = ", ")
}

get_hashtags <- function(legacy) {
  tags <- pluck(legacy, "entities", "hashtags")
  if (is.null(tags) || length(tags) == 0) return(NA_character_)
  collapse_chr(map_chr(tags, ~ safe_chr(.x, "text", default = NA_character_)))
}

get_urls <- function(legacy) {
  u <- pluck(legacy, "entities", "urls")
  if (is.null(u) || length(u) == 0) return(NA_character_)
  collapse_chr(map_chr(u, ~ safe_chr(.x, "expanded_url", default = NA_character_)))
}

get_mentions <- function(legacy) {
  m <- pluck(legacy, "entities", "user_mentions")
  if (is.null(m) || length(m) == 0) return(NA_character_)
  collapse_chr(map_chr(m, ~ safe_chr(.x, "screen_name", default = NA_character_)))
}

get_media <- function(legacy) {
  media <- pluck(legacy, "extended_entities", "media")
  if (is.null(media) || length(media) == 0) media <- pluck(legacy, "entities", "media")
  if (is.null(media) || length(media) == 0) return(list(images = NA_character_, videos = NA_character_))
  
  imgs <- keep(media, ~ safe_chr(.x, "type") == "photo")
  vids <- keep(media, ~ safe_chr(.x, "type") %in% c("video", "animated_gif"))
  
  img_urls <- map_chr(imgs, ~ safe_chr(.x, "media_url_https", default = NA_character_))
  
  vid_urls <- map_chr(vids, function(v) {
    variants <- pluck(v, "video_info", "variants")
    if (is.null(variants)) return(NA_character_)
    mp4s <- keep(variants, ~ safe_chr(.x, "content_type") == "video/mp4")
    if (length(mp4s) == 0) return(NA_character_)
    bitrates <- map_dbl(mp4s, ~ safe_num(.x, "bitrate", default = 0))
    safe_chr(mp4s[[which.max(bitrates)]], "url", default = NA_character_)
  })
  
  list(images = collapse_chr(img_urls), videos = collapse_chr(vid_urls))
}

strip_html <- function(x) {
  if (is.na(x)) return(x)
  str_trim(str_remove_all(x, "<[^>]+>"))
}

# ---- extract the raw search term (q=...) from the captured search URL ----
extract_search_query <- function(url) {
  if (is.na(url)) return(NA_character_)
  m <- str_match(url, "[?&]q=([^&]+)")[, 2]
  if (is.na(m)) return(NA_character_)
  URLdecode(str_replace_all(m, "\\+", " "))
}




# ---- parse a single ndjson line/item into a one-row tibble ----
parse_item <- function(item) {
  tw <- item[["data"]]
  if (is.null(tw)) return(NULL)
  
  legacy <- tw[["legacy"]]
  if (is.null(legacy)) return(NULL)
  
  screen_name <- safe_chr(tw, "core", "user_results", "result", "core", "screen_name")
  tweet_id    <- coalesce(safe_chr(legacy, "id_str"), safe_chr(tw, "rest_id"))
  
  note_text <- safe_chr(tw, "note_tweet", "note_tweet_results", "result", "text")
  full_text <- coalesce(note_text, safe_chr(legacy, "full_text"))
  
  media <- get_media(legacy)
  
  rt_result <- pluck(tw, "legacy", "retweeted_status_result", "result")
  is_rt     <- !is.null(rt_result)
  rt_user   <- if (is_rt) safe_chr(rt_result, "core", "user_results", "result", "core", "screen_name") else NA_character_
  
  qt_result   <- pluck(tw, "quoted_status_result", "result")
  is_qt       <- isTRUE(safe_lgl(legacy, "is_quote_status")) || !is.null(qt_result)
  qt_screen   <- safe_chr(qt_result, "core", "user_results", "result", "core", "screen_name")
  qt_name     <- safe_chr(qt_result, "core", "user_results", "result", "core", "name")
  qt_legacy   <- pluck(qt_result, "legacy")
  qt_note     <- safe_chr(qt_result, "note_tweet", "note_tweet_results", "result", "text")
  qt_text     <- coalesce(qt_note, safe_chr(qt_legacy, "full_text"))
  qt_media    <- if (!is.null(qt_legacy)) get_media(qt_legacy) else list(images = NA_character_, videos = NA_character_)
  
  is_reply     <- !is.na(safe_chr(legacy, "in_reply_to_status_id_str"))
  replied_user <- safe_chr(legacy, "in_reply_to_screen_name")
  
  coords    <- pluck(legacy, "coordinates", "coordinates")
  long_lat  <- if (!is.null(coords)) paste(unlist(coords), collapse = ",") else NA_character_
  place_nm  <- safe_chr(legacy, "place", "full_name")
  
  # --- tweet creation time ---
  created_at_chr <- safe_chr(legacy, "created_at")
  created_at_ts  <- suppressWarnings(
    as.POSIXct(created_at_chr, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC")
  )
  
  # --- scrape/collection time (Zeeschuimer capture metadata, ms since epoch) ---
  collected_at_ts <- suppressWarnings(
    as.POSIXct(safe_num(item, "timestamp_collected") / 1000, origin = "1970-01-01", tz = "UTC")
  )
  
  search_url <- safe_chr(item, "source_platform_url")
  
  tibble(
    id                 = tweet_id,
    thread_id          = safe_chr(legacy, "conversation_id_str"),
    created_at         = created_at_ts,     # tweet's original posting time
    collected_at       = collected_at_ts,   # when Zeeschuimer scraped it
    link               = if (!is.na(screen_name) && !is.na(tweet_id))
      paste0("https://x.com/", screen_name, "/status/", tweet_id) else NA_character_,
    subject            = NA_character_,
    body               = full_text,
    author             = screen_name,
    author_fullname    = safe_chr(tw, "core", "user_results", "result", "core", "name"),
    author_id          = safe_chr(tw, "core", "user_results", "result", "rest_id"),
    author_followers   = safe_num(tw, "core", "user_results", "result", "legacy", "followers_count"),
    source             = strip_html(safe_chr(tw, "source")),
    language_guess     = safe_chr(legacy, "lang"),
    possibly_sensitive = safe_lgl(legacy, "possibly_sensitive"),
    retweet_count      = safe_num(legacy, "retweet_count"),
    reply_count        = safe_num(legacy, "reply_count"),
    like_count         = safe_num(legacy, "favorite_count"),
    quote_count        = safe_num(legacy, "quote_count"),
    impression_count   = safe_num(tw, "views", "count"),
    is_retweet         = is_rt,
    retweeted_user     = rt_user,
    is_quote_tweet     = is_qt,
    quoted_user        = qt_screen,
    quote_author       = qt_name,
    quote_body         = qt_text,
    quote_images       = qt_media$images,
    quote_videos       = qt_media$videos,
    is_reply           = is_reply,
    replied_user       = replied_user,
    hashtags           = get_hashtags(legacy),
    urls               = get_urls(legacy),
    images             = media$images,
    videos             = media$videos,
    mentions           = get_mentions(legacy),
    long_lat           = long_lat,
    place_name         = place_nm,
    verified           = safe_lgl(tw, "core", "user_results", "result", "is_blue_verified"),
    promoted           = isTRUE(safe_lgl(tw, "promoted")),
    search_query       = extract_search_query(search_url)
  )
}

ndfull_x_20260818 <- map_dfr(raw_items, parse_item) %>%
  distinct(id, .keep_all = TRUE) %>%
  as_tibble()


# why my data has 30 052 rows when zeeschuimer says 41 131? #### 
n_lines <- length(lines)

# how many items lack a "data" field or a "legacy" field?
n_no_data   <- sum(map_lgl(raw_items, ~ is.null(.x[["data"]])))
n_no_legacy <- sum(map_lgl(raw_items, ~ !is.null(.x[["data"]]) && is.null(.x[["data"]][["legacy"]])))

# parse everything WITHOUT dropping nulls or deduping
parsed_list   <- map(raw_items, parse_item)
n_null_rows   <- sum(map_lgl(parsed_list, is.null))
n_parsed_rows <- sum(!map_lgl(parsed_list, is.null))

# how many duplicate tweet IDs exist among the parsed rows?
all_rows   <- map_dfr(raw_items, parse_item)   # no distinct() here
n_total    <- nrow(all_rows)
n_distinct <- n_distinct(all_rows$id)
n_dupes    <- n_total - n_distinct

cat("ndjson lines:      ", n_lines, "\n")
cat("missing data field:", n_no_data, "\n")
cat("missing legacy:    ", n_no_legacy, "\n")
cat("parsed OK:         ", n_parsed_rows, "\n")
cat("dropped (NULL):    ", n_null_rows, "\n")
cat("rows before dedup: ", n_total, "\n")
cat("unique IDs:        ", n_distinct, "\n")
cat("duplicate rows:    ", n_dupes, "\n")

all_rows |> count(id) |> arrange(-n)

all_rows |> filter(id == "2017241420554277251") |> View()
ndfull_x_20260818 |> filter(id == "2017241420554277251") |> View()
# ndfull has the oldest capture date
all_rows |> filter(id == "2026039363130560882") |> View()
ndfull_x_20260818 |> filter(id == "2026039363130560882") |> View()
# confirmed - the oldest capture date, good 

# note what promoted content (from period before Moltbook launch) is in the scraped data #### 

ndfull_x_20260818 |> count(promoted)
ndfull_x_20260818 |> filter(promoted == TRUE) |> View()

library(sjmisc)

ndfull_x_20260818 |> filter(promoted == TRUE) |> 
  select(author) |> 
  frq(sort.frq = "desc", min.frq = 2)

frq(ndfull_x_20260818$promoted) # less than 1% 

# let's check who is who 

# Value           |  N | Raw % | Valid % | Cum. %
# -----------------------------------------------
# thejsnation     | 21 |  7.64 |    7.64 |   7.64
# AikidoSecurity  | 17 |  6.18 |    6.18 |  13.82
# FundacjaAP      | 15 |  5.45 |    5.45 |  19.27
# ING__Polska     |  8 |  2.91 |    2.91 |  22.18
# BetclicPolska   |  7 |  2.55 |    2.55 |  24.73
# Akademia_WSB    |  6 |  2.18 |    2.18 |  26.91
# Coding_Creed    |  6 |  2.18 |    2.18 |  29.09
# FTMO_com        |  6 |  2.18 |    2.18 |  31.27
# GrupaMtp        |  6 |  2.18 |    2.18 |  33.45
# eBiletPL        |  5 |  1.82 |    1.82 |  35.27
# eduardmirica    |  5 |  1.82 |    1.82 |  37.09
# TradexWhisperer |  5 |  1.82 |    1.82 |  38.91
# Y_J_Inquisitor  |  5 |  1.82 |    1.82 |  40.73

# Twitter handle lookup table
# Value           |  N | Official URL                                | Description
# --------------------------------------------------------------------------------------------
# thejsnation     | 21 | https://twitter.com/thejsnation             | JS Nation - JavaScript-focused international tech conference (GitNation family)
# AikidoSecurity  | 17 | https://twitter.com/AikidoSecurity           | Aikido Security - Belgian app security platform (SAST, dependency/vuln scanning)
# FundacjaAP      | 15 | https://x.com/FundacjaAP                     | Fundacja Academic Partners - Polish non-profit organizing IT events (Warsaw IT Days, Hack Summit)
# ING__Polska     |  8 | https://twitter.com/ING__Polska              | ING Bank Slaski - Polish arm of ING Group, major retail/corporate bank
# BetclicPolska   |  7 | https://twitter.com/BetclicPolska            | Betclic Polska - Polish branch of Betclic online sports betting company
# Akademia_WSB    |  6 | https://twitter.com/Akademia_WSB             | WSB University - private Polish university (business, IT programs)
# Coding_Creed    |  6 | https://twitter.com/codingcreed              | Coding Creed - software/web dev studio (custom software, UX/UI, TS/React tools)
# FTMO_com        |  6 | https://twitter.com/FTMO_com                 | FTMO - Czech prop trading firm funding forex/futures traders
# GrupaMtp        |  6 | https://twitter.com/GrupaMtp                 | Grupa MTP - Poznan International Fair, major Polish trade fair organizer
# eBiletPL        |  5 | https://twitter.com/eBiletPL                 | eBilet.pl - leading Polish online ticketing platform (concerts, events, sports)
# eduardmirica    |  5 | https://x.com/eduardmirica                   | Eduard Mirica - Romanian VR/AR artist and indie game developer
# TradexWhisperer |  5 | https://x.com/TradexWhisperer                | Trade Whisperer - finance/markets commentator, stock analysis and chart signals
# Y_J_Inquisitor  |  5 | https://x.com/Y_J_Inquisitor                 | Personal X account; limited public info available on owner/focus

# tech conferences and companies, banks and trading, betting, 
# tech and finance influencers, private education - better keep them and filter just by date?  

# use tweets from Moltbook lanunch to the end of July #### 

# . Moltbook launched on January 28, 2026 
nd <- ndfull_x_20260818 |> filter(between(x = created_at, left = as_datetime("2026-01-28 00:00:00"), right = as_datetime("2026-07-31 23:59:59")))

outside_time_period <- nrow(ndfull_x_20260818) - nrow(nd)
cat(outside_time_period, "obs. outside time period, ", outside_time_period / nrow(ndfull_x_20260818) * 100, "% of all obs.")
# only 1.56 % of observations are outside time period 

summary(nd$created_at)

ggplot(nd, aes(x = created_at)) +
  geom_freqpoly(binwidth = 60 * 60 * 24) +      # daily
  theme_minimal()

ggplot(nd, aes(x = created_at)) +
  geom_freqpoly(binwidth = 60 * 60 * 24 * 7) +  # weekly
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

# let's see the first post

nd |> filter(created_at == as_datetime("2026-01-28 07:14:26")) |> View() 
# promoted from the jsnation, not about moltbook

nd |> slice_min(created_at, n = 10) |> View() 
# Matt Schlicht — twórca Moltbook, w biznesie AI Agents od paru lat. 
# Jego tweety są w moich danych pierwszymi merytorycznymi o Moltbook: thread_id 2016560277333168540

# OK, nd is final dataset for ESA #### 

# save(nd, file = "nd.RData")

