Sys.setlocale("LC_TIME", "en_US.UTF8")
library(ggmap)
library(tidyverse)
library(lubridate)

dat <- read_csv("data/week10_biketown.csv") %>%
  # The type of StartDate isn't guessed right
  mutate(StartDate = mdy(StartDate)) %>%
  # A column with weekdays will be useful later
  mutate(start_day = weekdays(StartDate)) %>%
  mutate(start_hour = hour(StartTime))
  
# Map inspired by https://twitter.com/WireMonkey/status/1004790383451176962
portland_path <- "data/portland_map.Rdata" 
if(!file.exists(portland_path)) {
portland <- get_map("Portland, Oregon",
                    zoom = 13,
                    # source = "stamen",
                    maptype = "toner-lite")
save(portland, file = portland_path)
} else {
  load(portland_path)
}

ggmap(portland) +
  geom_count(data = dat %>% 
               select(StartLatitude, StartLongitude),
             aes(x = StartLongitude,
                 y = StartLatitude))
