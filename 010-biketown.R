Sys.setlocale("LC_TIME", "en_US.UTF8")
library(ggmap)
library(tidyverse)
library(lubridate)


# Get data ----------------------------------------------------------------

dat <- read_csv("data/week10_biketown.csv", 
                # read only the columns that you need
                col_types = cols_only(StartHub = col_character(),
                                      StartLatitude = col_double(),
                                      StartLongitude = col_double(),
                                      StartTime = col_time(format = ""),
                                      StartDate = col_character())) %>%
  filter(complete.cases(.)) %>%
  # This is the only way I managed to get the correct date
  mutate(StartDate = mdy(StartDate)) %>%
  # columns useful later
  mutate(start_day = weekdays(StartDate)) %>%
  mutate(start_hour = hour(StartTime)) %>%
  mutate(start_month = month(StartDate, label = TRUE)) %>%
  mutate(start_month = factor(start_month, ordered = F)) %>%
  # in the end I decided to use both month and year for the GIF
  mutate(start_at = as_factor(paste(start_month,
                                    year(StartDate))))
  

# Get Map -----------------------------------------------------------------

# Map inspired by https://twitter.com/WireMonkey/status/1004790383451176962
portland_path <- "data/portland_map.Rdata" 
if(!file.exists(portland_path)) {
# portland <- get_map("Portland, Oregon",
  portland <- get_map(location = c(-122.710, 45.49,
                                   -122.615, 45.57),
                    zoom = 13,
                    # source = "stamen",
                    maptype = "toner-lite")
save(portland, file = portland_path)
} else {
  load(portland_path)
}

# Make gif with months ----------------------------------------------------

# a small transformation necessary because
# I did not manage to make gganimate work
# on geom_count

dat <- dat %>%
  group_by(StartHub) %>%
  mutate(lat = mean(StartLatitude),
         lon = mean(StartLongitude),
         start_at = start_at) %>%
  group_by(start_at, StartHub, lat, lon) %>%
  summarise(n = n())


# inspired by https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
library(gganimate)
p <- ggmap(portland) +
  geom_point(data = dat,
             aes(x = lon,
                 y = lat,
                 size = n,
                 frame = start_at),
             # alpha = .3,
             colour = "blue") +
  ggtitle("Bikesharing ride starts in Portland") +
  theme(text = element_text(size = 18))
  
gganimate(p,
          filename = "plots/portland.gif",
          ani.width=600, ani.height=700)

