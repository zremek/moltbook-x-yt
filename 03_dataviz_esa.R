library(tidyverse)
library(sjPlot)

yt_esa <- classification_yt_job1

cat("n =", nrow(x_esa), "is the x.com sample size for ESA")
cat("n =", nrow(yt_esa), "is the YT sample size for ESA")


# freqpoly - content voulume in time #### 

ggplot(x_esa, aes(x = created_at)) +
  geom_freqpoly(binwidth = 60 * 60 * 24 * 7) +  # weekly
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

ggplot(yt_esa, aes(x = publishedAt)) +
  geom_freqpoly(binwidth = 60 * 60 * 24 * 7) +  # weekly
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

# one tibble for both sources #### 

both_esa <- bind_rows(
  x_esa |> select(id, created_at, fake_or_real_class) |> 
    mutate(id = as.character(id), 
           source = "x.com"), 
  yt_esa |> select(id, publishedAt, fake_or_real_class) |> 
    rename(created_at = publishedAt) |> 
    mutate(source = "YouTube")
)

both_esa |> count(source)

# freqpoly for both - content voulume in time #### 

ggplot(both_esa, aes(x = created_at, colour = source)) +
  geom_freqpoly(binwidth = 60 * 60 * 24 * 7) +  # weekly
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") 

ggplot(both_esa, aes(x = created_at, colour = source)) +
  geom_freqpoly(binwidth = 60 * 60 * 24) +  # daily
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") 

# stacked bars for classification #### 

plot_grpfrq(both_esa$fake_or_real_class, 
            both_esa$source, bar.pos = "stack")

plot_xtab(both_esa$source, 
          both_esa$fake_or_real_class, 
          bar.pos = "stack", 
          margin = "row", show.n = F, rev.order = T) + 
  coord_flip() + 
  theme_minimal()

# TODO add proper labels, don't adjust colours

plot_xtab(both_esa$source, 
          both_esa$fake_or_real_class, 
          bar.pos = "stack", 
          margin = "row", show.n = F, rev.order = T, 
          legend.labels = c("no", "yes", "uncertain"), 
          legend.title = "content discusses Moltbook as real or fake?") + 
  coord_flip() + 
  theme_minimal() + 
  labs(caption = "own elaboration")


ggplot(both_esa, aes(x = created_at, colour = source)) +
  geom_freqpoly(binwidth = 60 * 60 * 24) +  # daily
  theme_minimal() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "content created at [2026 daily series]", 
       caption = "own elaboration")
