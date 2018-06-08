library(ggmap)
library(tidyverse)

dat <- read_csv("data/week10_biketown.zip")

# Map inspired by https://twitter.com/WireMonkey/status/1004790383451176962
portland <- get_map("Portland, Oregon",
                    zoom = 13,
                    # source = "stamen",
                    maptype = "toner-lite")
ggmap(portland) +
  geom_point(data = dat %>% 
               select(StartLatitude, StartLongitude),
             aes(x = StartLongitude,
                 y = StartLatitude))
