Sys.setlocale("LC_TIME", "en_US.UTF8")
library(ggmap)
library(tidyverse)
library(lubridate)


# Get data ----------------------------------------------------------------

dat <- read_csv("data/week10_biketown.csv") %>%
  # The type of StartDate isn't guessed right
  mutate(StartDate = mdy(StartDate)) %>%
  # A column with weekdays will be useful later
  mutate(start_day = weekdays(StartDate)) %>%
  mutate(start_hour = hour(StartTime)) %>%
  mutate(start_month = as.factor(month(StartDate)))
  

# Get Map -----------------------------------------------------------------

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


# Test subset -------------------------------------------------------------

jpeg(filename = "plots/portland-weekdays-hours.jpeg",
     width = 4000, height = 8000)
ggmap(portland) +
  geom_count(data = dat,
             aes(x = StartLongitude,
                 y = StartLatitude)) +
  facet_grid(start_hour ~ start_day)
dev.off()

jpeg(filename = "plots/portland-month.jpeg",
     width = 8000, height = 1000)
ggmap(portland) +
  geom_count(data = dat,
             aes(x = StartLongitude,
                 y = StartLatitude)) +
  facet_grid(. ~ start_month)
dev.off()


jpeg(filename = "plots/portland-weekdays.jpeg",
     width = 8000, height = 1000)
ggmap(portland) +
  geom_count(data = dat,
             aes(x = StartLongitude,
                 y = StartLatitude)) +
  facet_grid(. ~ start_day)
dev.off()


# Make gif with months ----------------------------------------------------
dat <- dat %>%
  group_by(start_month, StartLatitude, StartLongitude) %>%
  summarise(n = n())


# inspired by https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
library(gganimate)
p <- ggmap(portland) +
  geom_point(data = dat,
             aes(x = StartLongitude,
                 y = StartLatitude,
                 size = n,
                 frame = start_month),
             alpha = .3) 
  

# p$mapping <- aes(x = lon,
#                  y = lat,
#                  frame = start_month)

gganimate(p)
