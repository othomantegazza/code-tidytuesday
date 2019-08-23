library(tidyverse)
library(lubridate)
library(grid)
library(tibbletime)
library(maptools)
library(sf)

bg_col <- "#3752C3"
fill_col <- "#5867A6"

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-08-20/nuclear_explosions.csv")

data_path <- 'data/2-34-nukes.Rdata'


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  nukes <- 
    data_url %>% 
    read_csv(col_types = cols(
      date_long = col_datetime(format = "%Y%m%d")
    )) %>% 
    mutate(month = month(date_long))
  
  save(nukes, file = data_path)
} else {
  load(data_path)
}


# plot --------------------------------------------------------------------

nukes %>% 
  ggplot() +
  borders(fill = fill_col, colour = bg_col, size = .4) +
  geom_point(aes(x = longitude,
                 y = latitude,
                 size = yield_lower),
             colour = "yellow",
             alpha = .05) +
  coord_map(projection = "mollweide", orientation = c(90, 0, 0)) +
  theme_void() +
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill = bg_col))


# months ------------------------------------------------------------------

my <- 
  nukes %>% 
  arrange(date_long) %>% 
  mutate(my = paste(year, month, "01", sep = "-")) %>% 
  pull(my) %>% unique()

nukes %>% 
  filter(date_long < as_date(my[5]))



# explore -----------------------------------------------------------------

nukes %>% 
  map(~is.na(.) %>% sum())

nukes %>% 
  ggplot(aes(x = date_long,
             y = magnitude_body)) +
  geom_point()


nukes %>% 
  ggplot(aes(x = date_long,
             y = magnitude_surface)) +
  geom_point()


nukes %>% 
  ggplot(aes(x = yield_upper,
             y = magnitude_body)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() 

nukes %>% pull(type) %>% unique

nukes %>% 
  mutate(u = case_when(depth >= 0 ~ TRUE, TRUE ~ FALSE)) %>% 
  ggplot(aes(x = year,
             fill = u)) +
  geom_bar()



# cumulative plot ---------------------------------------------------------


year_states <- 
  expand.grid(year = min(nukes$year):max(nukes$year),
              country = unique(nukes$country), stringsAsFactors = FALSE)

nukes2 <- 
  nukes %>% 
  count(year, country) %>% 
  group_by(country) %>% 
  mutate(n_cumulative = cumsum(n)) %>% 
  select(-n) %>% 
  bind_rows(expand.grid(year = min(nukes$year) - 1,
              country = unique(nukes$country),
              n_cumulative = 0,
              stringsAsFactors = FALSE)) %>% 
  full_join(year_states) %>% 
  arrange(country, year) %>% 
  fill(n_cumulative, .direction = "down")

nukes2 %>% 
  ggplot(aes(x = year,
             y = n_cumulative,
             colour = country)) +
  geom_point() +
  geom_smooth()


# many plots --------------------------------------------------------------

# plot_nukes <- function(my_in) {
#   p <-
#     nukes %>%
#     filter(date_long <= as_date(my_in))
#   
#   print(p)
#   
#   d2 <-
#     nukes %>%
#     filter(month == month(my_in),
#            year == year(my_in))
#   
#   print(d2)
#   
#   p <- p %>%
#     ggplot() +
#     borders(fill = fill_col,
#             colour = bg_col,
#             size = .4) +
#     {
#       if (nrow(d2) > 0) {
#         geom_point(
#           data = d2 ,
#           aes(x = longitude,
#               y = latitude,
#               size = yield_lower),
#           size = 5,
#           fill = "yellow"
#         )
#       }
#     } +
#     geom_point(
#       aes(x = longitude,
#           y = latitude,
#           size = yield_lower),
#       colour = "yellow",
#       alpha = .05
#     ) +
#     coord_map(projection = "mollweide", orientation = c(90, 0, 0)) +
#     theme_void() +
#     guides(size = FALSE) +
#     theme(panel.background = element_rect(fill = bg_col))
#   
#   print(my_in)
#   
#   ggsave(paste0("plots/2-34-nukes/", my_in, ".png"),  p)
# }


