library(tidyverse)
library(lubridate)
library(ggridges)
library(tibbletime)
library(cowplot)


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

to_plot <- 
  bike_traffic %>% 
  rename(time_stamp = "date") %>% 
  group_by(crossing, time_stamp) %>% 
  summarise(bike_count = sum(bike_count)) %>% 
  ungroup() %>% 
  mutate(year = year(time_stamp),
         year_day = yday(time_stamp),
         week_day = wday(time_stamp),
         day_hour = hour(time_stamp),
         date_day = round_date(time_stamp, unit = "day")) %>% 
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
  to_plot %>% pull(time_stamp) %>% 
  unique() %>%
  # {tibble(year = year(.),
  #         year_day = yday(.),
  #         month = month(., label = TRUE),
  #         week_day = wday(.))} %>%
  {tibble(time_stamp = round_date(., unit = "day"))} %>% 
  distinct() %>% 
  mutate(year_week = epiweek(time_stamp), # in other cases check isoweek()
         week_day = wday(time_stamp, label = TRUE)) %>% 
  filter(year(time_stamp) == 2017) %>% 
  mutate(year_week = case_when(year_week == 1 & month(time_stamp) == 12 ~ 52,
                               TRUE ~ year_week)) %>% 
  # split by week day to add a row after each Saturday
  {split(., .$year_week)} %>% 
  map(~mutate(.,new_month = month(time_stamp) %>% unique %>% length()) %>% 
        bind_rows(., c(time_stamp = NA_real_,
                       year_week = NA_real_,
                       week_day = NA_real_,
                       new_month = NA_real_))) %>% 
  reduce(bind_rows)  %>% 
  mutate(label = case_when(is.na(time_stamp) ~ TRUE,
                           TRUE ~ FALSE),
         new_month = new_month - 1) %>% 
  fill(time_stamp, new_month, .direction = "down") %>%
  # small fix to filled values
  mutate(time_stamp = case_when(label ~ time_stamp + days(1),
                                TRUE ~ time_stamp)) %>% 
  select(-year_week, -week_day) %>% 
  rename(day_in = "time_stamp") %>% 
  # small first day
  add_row(day_in = as_date(NA_real_),
          new_month = 1, label = TRUE, .before = 1) %>% 
  fill(day_in, .direction = "up")

# 230: day with unusually high counts
ytop <-
  to_plot %>% 
  filter(year_day != 230) %>% 
  pull(s_mean) %>% max(na.rm = TRUE)

plot_bikes <- function(day_in, new_month, label)
{
  if(!label) {
    p <- to_plot %>%
      filter(yday(time_stamp) == yday(day_in)) %>% 
      ggplot(aes(x = day_hour,
                 fill = crossing,
                 colour = crossing)) +
      geom_ribbon(aes(ymin = 0,
                      ymax = s_mean),
                  alpha = .2) +
      guides(colour = FALSE,
             fill = FALSE) +
      lims(y = c(0, ytop)) +
      theme_void()
    
    return(p)
  } else if(new_month) {
    tibble(x = 1, y = c(0,1), to_write = c(day_in, "")) %>% 
    ggplot(aes(x=x, y=y, label = paste(month(to_write, label = T),
                                       mday(to_write)))) +
      geom_text(vjust = 0) +
      ylim(0, .7) +
      theme_void()
  } else {
    ggplot() + theme_void()
  }
}

p_list <- 
  looper %>% 
  pmap(plot_bikes)



# gtable ------------------------------------------------------------------

library(gtable)

a <- p_list[[2]]
b <- p_list[[34]]

rbind(a %>% ggplotGrob(), b %>% ggplotGrob())


# viewport ----------------------------------------------------------------

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

vplayout <- function(x, y)  viewport(layout.pos.row = x, layout.pos.col = y)

print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(b, vp = vplayout(2, 1))
print(a, vp = vplayout(2, 2))

# use grid directly? ------------------------------------------------------
library(grid)

tst <- p_list[[3]]
tst

grid.ls()
tst %>% ggplotGrob()


# with cowplot ------------------------------------------------------------

length(p_list)/2


p_panel <- 
  cowplot::plot_grid(plotlist = p_list[1:207],
                     ncol = 8)

ggdraw() + draw_plot(p_panel %>% plot_to_gtable(), x = .1, y = .1, scale = .8)

png(filename = "plots/2-14-seattle-bikes.png",
    height = 7000, width = 1600,
    res = 300)
# p_panel
ggdraw() + draw_plot(p_panel, x = .1, y = .1, width = .8, height = .8)
dev.off()

# p <- 
#   tst %>% #pull(crossing) %>%  unique()
#   # filter(crossing == "Burke Gilman Trail") %>% 
#   # slice(25:48) %>% #View
#   # crazy high count
#   filter(year_day != 230) %>%  #View()
#   # filter(year_day > 45,year_day < 50) %>%
#   ggplot(aes(x = day_hour,
#              fill = crossing,
#              colour = crossing)) +
#   geom_ribbon(aes(ymin = 0, ymax = s_mean),
#               alpha = .2) +
#   # geom_point() +
#   # geom_smooth() +
#   # geom_density(aes(y = ..count..)) +
#   guides(colour = FALSE,
#          fill = FALSE) +
#   facet_wrap(facets = "year_day", ncol = 7) +
#   theme_minimal()

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
