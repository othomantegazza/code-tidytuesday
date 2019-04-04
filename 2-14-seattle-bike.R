library(tidyverse)
library(lubridate)
library(ggridges)
library(tibbletime)


# weekdays in english
Sys.setlocale("LC_TIME", "en_US.UTF8")

# bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
# 
# save(bike_traffic, file = "data/2-14-seattle-bike.Rdata")

load("data/2-14-seattle-bike.Rdata")

bike_traffic <- 
  bike_traffic %>% 
  mutate(date = mdy_hms(date))

# explore -----------------------------------------------------------------

bike_traffic %>% 
  pull(crossing) %>% 
  unique()

bike_traffic %>% 
  mutate(year_day = yday(.data$date),
         year = year(.data$date)) %>% 
  group_by(year, year_day, crossing) %>% 
  summarise(bike_count = sum(bike_count, na.rm = TRUE))

bike_traffic %>% 
  ggplot(aes(x = bike_count)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(crossing ~ .)



# parameters --------------------------------------------------------------

color_points <- "#3752C3"


# ridges ------------------------------------------------------------------

# meh
bike_traffic %>% 
  mutate(year_day = yday(.data$date),
         year = year(.data$date)) %>% 
  group_by(year, year_day, crossing) %>% 
  summarise(bike_count = sum(bike_count, na.rm = TRUE)) %>% 
  filter(year > 2013,
         year < 2019) %>% #arrange(desc(bike_count)) %>%  top_n(n = 10)
  ggplot(aes(x = bike_count,
             y = year %>% as.character() %>% as.factor())) +
  geom_density_ridges(rel_min_height = .02,
                      scale = .9,
                      quantile_lines = TRUE,
                      colour = color_points,
                      fill = "grey70",
                      # fill = "#263A89",
                      alpha = .5) +
  scale_x_sqrt() +
  # lims(x = c(0, 4000)) +
  facet_wrap(facets = "crossing", scales = "free_x", nrow = 1) +
  theme_bw()

bike_traffic %>% 
  mutate(week_day = wday(.data$date),
         week = week(.data$date), 
         year = year(.data$date)) %>% 
  # group_by(year, year_day, crossing) %>% 
  filter(year > 2013,
         year < 2019) #arrange(desc(bike_count)) %>%  top_n(n = 10)


roll_mean <- rollify(window = 3, .f = mean)

tst <- 
  bike_traffic %>% 
  group_by(crossing, date) %>% 
  summarise(bike_count = sum(bike_count)) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         year_day = yday(date),
         week_day = wday(date),
         day_hour = hour(date)) %>% 
  filter(year == 2017) %>%
  # filter(year_day > 45,year_day < 50) %>% 
  group_by(crossing) %>%
  arrange(year_day) %>% 
  mutate(s_mean = roll_mean(bike_count)) %>% 
  # mutate(bike_count = case_when(is.na(bike_count) ~ 0,
  #                               TRUE ~ bike_count),
  #        smooth_counts = smooth(bike_count)) %>%
  ungroup()

# make a dataframe to loop on
looper <- 
  tst %>% pull(date) %>% 
  unique() %>%
  # {tibble(year = year(.),
  #         year_day = yday(.),
  #         month = month(., label = TRUE),
  #         week_day = wday(.))} %>%
  {tibble(date = round_date(., unit = "day"))} %>% 
  distinct() %>% 
  mutate(year_week = epiweek(date), # in other cases check isoweek()
         week_day = wday(date, label = TRUE)) %>% 
  filter(year(date) == 2017) %>% 
  mutate(year_week = case_when(year_week == 1 & month(date) == 12 ~ 52,
                               TRUE ~ year_week)) %>% 
  # split by week day to add a row after each Saturday
  {split(., .$year_week)} %>% 
  map(~bind_rows(., c(date = NA_real_,
                 year_week = NA_real_,
                 week_day = NA_real_))) %>% 
  reduce(bind_rows)  %>% 
  mutate(label = case_when(is.na(date) ~ TRUE,
                                 TRUE ~ FALSE)) %>% 
  fill(date) %>% 
  mutate(date = case_when(label ~ date + days(1),
                          TRUE ~ date))
  


p <- 
  tst %>% #pull(crossing) %>%  unique()
  # filter(crossing == "Burke Gilman Trail") %>% 
  # slice(25:48) %>% #View
  # crazy high count
  filter(year_day != 230) %>%  #View()
  # filter(year_day > 45,year_day < 50) %>%
  ggplot(aes(x = day_hour,
             fill = crossing,
             colour = crossing)) +
  geom_ribbon(aes(ymin = 0, ymax = s_mean),
              alpha = .2) +
  # geom_point() +
  # geom_smooth() +
  # geom_density(aes(y = ..count..)) +
  guides(colour = FALSE,
         fill = FALSE) +
  facet_wrap(facets = "year_day", ncol = 7) +
  theme_void()

# p

# png(filename = "plots/2-14-seattle-bikes.png",
#     height = 4000, width = 900,
#     res = 3000)
# p
# dev.off()


svglite::svglite(file = "plots/2-14-seattle-bikes.svg",
                 width = 7,
                 height = 30)
p
dev.off()
