# YouTube Data Tools — Video List
# Run: 2026-08-24 16:46:56 → 2026-08-24 16:47:01
# 
# Parameters:
#   query: moltbook
# order: relevance
# 
# Result: 399 videos
# API usage: 18 calls, ~909 quota units
# 
# Files (available for seven days):
#   https://10.10.20.191/files/videolist_search399_2026_08_24-16_47_01.csv (399 rows)

library(tidyverse)

videolist <- read_csv("videolist_search399_2026_08_24-16_47_01.csv")

top10views <- videolist |> 
  slice_max(viewCount, n = 10) |> 
  select(channelTitle, videoId, videoTitle, videoDescription, publishedAt, viewCount, commentCount)

sum(top10views$commentCount) # 14192 comments in 10 videos 

# YouTube Data Tools — Video Comments
# Run: 2026-08-24 17:05:29 → 2026-08-24 17:05:39
# 
# Parameters:
#   ids: YFjfBk8HI5o, 2PWFj50DcZU, SWV5SX2sW9M, 6OXE65fjjsU, NMzxOe3Po6M, 5Vsv_LBTTk0, S3z4be-lp-0, WbQJSUTmTbA, F22iV2RzUus, -fmNzXCp7zA
# 
# Result: 14,271 comments, 11,889 users
# API usage: 262 calls, ~262 quota units
# 
# Files (available for seven days):
#   https://10.10.20.191/files/videocomments_bulk_seeds10_comments_2026_08_24-17_05_39.csv (14,271 rows)
# https://10.10.20.191/files/videocomments_bulk_seeds10_usernetwork_nodes11889_2026_08_24-17_05_39.gexf (11,889 nodes, 3,456 edges)

comments_full <- read_csv("videocomments_bulk_seeds10_comments_2026_08_24-17_05_39.csv")

# use comments from Moltbook lanunch to the end of July #### 

# . Moltbook launched on January 28, 2026 
comments <- comments_full |>
  filter(between(x = publishedAt, 
                 left = as_datetime("2026-01-28 00:00:00"), 
                 right = as_datetime("2026-07-31 23:59:59")))

nrow(comments)

# fake / real comments "yes" examples:
# 127c1b3e7f71c09dab1cb4ef0713da87f44e4196 
# d44d6cee7d95d64e54d94392c53323d608cd71e7 

comments |> filter(id %in% c("127c1b3e7f71c09dab1cb4ef0713da87f44e4196", 
                             "d44d6cee7d95d64e54d94392c53323d608cd71e7")) |> 
  select(videoId, id, likeCount, text) |> 
  View()

# comments is the dataset for ESA #### 




