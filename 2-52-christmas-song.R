library(tidyverse)
library(lubridate)

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
  mutate(weekid = mdy(weekid))
  
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
  # january hits belong to the year before
  mutate(year = year %>% {case_when(month < 6 ~ . - 1,
                                    TRUE ~ .)}) %>% 
  ggplot(aes(x = weekid,
             y = week_position,
             group = songid)) +
  geom_line() +
  facet_grid(. ~ year, scales = 'free_x', space = 'free_x') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
