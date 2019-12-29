library(tidyverse)
library(lubridate)
library(ggforce)

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

songs %>% 
  filter(year == 2011) %>%
  ggplot(aes(x = weekid,
             y = week_position,
             group = songid)) +
  geom_point() +
  geom_point(shape = 1, size = 3) +
  geom_line() +
  scale_y_reverse() +
  theme_minimal()

songs %>% 
  filter(songid == peak_10$songid[1]) %>%
  ggplot(aes(x = as.numeric(weekid),
             y = week_position,
             group = songid)) +
  # geom_point(size = 3) +
  geom_ellipse(aes(x0 = as.numeric(weekid),
                   y0 = week_position,
                   a = 2, b = 1,
                   angle = 1), fill = "black") +
  coord_fixed() +
  # geom_point(shape = 1, size = 3) +
  geom_hline(yintercept = 100, size = 3) +
  # scale_y_reverse() +
  theme_minimal()
  

  # Basic usage
  ggplot() +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 1, b = 3, angle = 0)) +
    coord_fixed()
  
  # Rotation
  # Note that it expects radians and rotates the ellipse counter-clockwise
  ggplot() +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 4)) +
    coord_fixed()
  
  # Draw a super ellipse
  ggplot() +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 3, angle = -pi / 3, m1 = 3)) +
    coord_fixed()