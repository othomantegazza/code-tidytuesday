library(tidyverse)
library(lubridate)
library(ggforce)
library(broom)

lyrics_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv"

songs_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv"

dest_path <- "data/52-christmas-song.Rdata"

if(!file.exists(dest_path)) {
  
  lyrics <- read_delim(lyrics_url, delim = "\t")
  songs <- read_csv(songs_url)
  
  save(lyrics, songs, file = dest_path)
  
} else{
  load(dest_path)
}

songs <- 
  songs %>% 
  mutate(weekid = mdy(weekid))  %>% 
  # january hits belong to the year before
  mutate(year = year %>% {case_when(month < 6 ~ . - 1,
                                  TRUE ~ .)})
  
# explore -----------------------------------------------------------------

songs %>% 
  distinct(song, peak_position) %>% 
  ggplot(aes(x = peak_position)) +
  geom_histogram()

# accidental art
songs %>% 
  ggplot(aes(x = weekid,
             y = week_position,
             group = songid)) +
  geom_line() +
  theme_void()

songs %>% 
  ggplot(aes(x = weekid,
             y = week_position,
             group = songid)) +
  geom_line() +
  facet_grid(. ~ year, scales = 'free_x', space = 'free_x') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# top songs ---------------------------------------------------------------

peak_10 <- 
  songs %>% 
  group_by(songid) %>% 
  filter(week_position == max(peak_position)) %>% 
  ungroup() %>% 
  arrange(peak_position) %>%
  distinct(song, performer, songid) %>% 
  slice(1:10)

# prototype ---------------------------------------------------------------

offset_y <- 80

songs %>% 
  filter(songid == peak_10$songid[1]) %>%
  {broom::augment(lm(week_position ~ weekid, data = .))} %>% 
  mutate(weekid = as.numeric(weekid),
         weekid_bar = weekid + 1.2) %>% 
  ggplot(aes(x = weekid,
             y = week_position)) +
  # geom_point(size = 3) +
  geom_hline(yintercept = 100,
             size = .1) +
  geom_ellipse(aes(x0 = as.numeric(weekid),
                   y0 = week_position,
                   a = 1.5, b = 1,
                   angle = 1), fill = "black") +
  geom_smooth(data = . %>% mutate(week_position = week_position + offset_y),
              mapping = aes(x = weekid_bar),
              method = "lm", se = FALSE, colour = "black") +
  # geom_segment(data = . %>% 
  #                summarize(min_weekid = min(weekid_bar),
  #                          max_weekid = max(weekid_bar),
  #                          y = ),
  #              mapping = aes(x = min_weekid,
  #                            xend = max_weekid,
  #                            y = 100,
  #                            yend = 100)) +
  geom_segment(aes(x = weekid_bar,
                   xend = weekid_bar,
                   yend = .fitted + offset_y)) +
  # geom_hline(yintercept = 100, size = 3) +
  coord_fixed(ratio = .4) +
  # geom_point(shape = 1, size = 3) +
  # scale_y_reverse() +
  theme_minimal()
