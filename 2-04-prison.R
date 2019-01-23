library(tidyverse)

# Get Data ----------------------------------------------------------------


dat_path <- "data/2-04-prison.Rdata"
# dat_url <- paste0("https://raw.githubusercontent.com/",
#                   "rfordatascience/tidytuesday/master/data/")
dat_url <- "~/Desktop/tidytuesday/data/2019/2019-01-22/prison_population.csv"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(dat_url)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}



# explore -----------------------------------------------------------------

# prison by category

dat %>% 
  ggplot(aes(x = prison_population)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(pop_category ~ .)

# not sure what to make of this,
#  I need to know the original popululation

# How does jail population changes over time?

dat_sum <- 
  dat %>% 
  filter(pop_category == "Total") %>% 
  group_by(year) %>% 
  summarise(prison_population = sum(prison_population, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>% 
  gather(key = "pop_type", value = "value", population, prison_population)

dat_sum %>%
  ggplot(aes(x = year,
             y = value,
             group = pop_type)) +
  geom_point() +
  geom_line() +
  scale_y_log10()

# log scale masks the real scale

dat_sum %>%
  ggplot(aes(x = year,
             y = value)) +
  geom_point() +
  geom_line() +
  facet_grid(pop_type ~ .,
             scales = "free_y")

# There is a sharp increase on 1984, why? 

dat_sum %>% 
  filter(year <= 1984) # %>% View()

# For some reason there are no record on prison population
# before 1983, cancel those years
# Also no record for 2016

dat_sum <- 
  dat_sum %>% 
  filter(year > 1982,
         year != 2016) 

# plot the ratio
dat_sum %>%  
  spread(key = pop_type, value = value) %>% 
  mutate(ratio = prison_population/population) %>% 
  ggplot(aes(x = year, y = ratio)) +
  geom_point()

# what if I use state level data
dat_state <- 
  dat %>% 
  filter(pop_category == "Total",
         year > 1982, 
         year != 2016) %>% 
  group_by(year, state) %>% 
  summarise(prison_population = sum(prison_population, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>% 
  mutate(ratio = prison_population/population)

dat_state %>% 
  ggplot(aes(x = year,
             y = ratio,
             group = state)) +
  geom_line()
  
# not clear, are there missing data in specific states?
# maybe a heatmap can help

dat_state %>% 
  ggplot(aes(x = year,
             y = state,
             fill = ratio)) +
  geom_raster() +
  scale_fill_viridis_c()

## What if I use jail populationn


dat_state %>% 
  ggplot(aes(x = year,
             y = state,
             fill = prison_population)) +
  geom_raster() +
  scale_fill_viridis_c(trans = "log")

## is it me who replaced NA with 0 when I used na.rm = TRUE?
## check with Arkansas

dat %>%
  filter(state == "AK") %>% 
  pull(prison_population) %>% 
  is.na() %>% all()

# yes


# Only on counties without na? --------------------------------------------

dat$prison_population %>% is.na() %>% sum()

tst <- 
  dat %>% 
  filter(pop_category == "Total",
         year > 1990,
         year != 2016) %>% 
  group_by(state, county_name) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()

tst %>%
  filter(is.na(prison_population)) %>% View()

tst_sum <- 
  tst %>% 
  group_by(year, state) %>% 
  summarise(prison_population = sum(prison_population),
            population = sum(population)) %>% 
  mutate(ratio = prison_population/population)

# tst_sum %>% 
#   ggplot(aes(x = year,
#              y = ratio,
#              group = state)) +
#   geom_line()

# not clear, are there missing data in specific states?
# maybe a heatmap can help

tst_sum %>% 
  ggplot(aes(x = year,
             y = state,
             fill = ratio)) +
  geom_raster() +
  scale_fill_viridis_c()

tst %>% 
  filter(pop_category == "Total") %>% 
  group_by(year) %>% 
  summarise(prison_population = sum(prison_population, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>% 
  gather(key = "pop_type", value = "value",
         population, prison_population) %>%
  ggplot(aes(x = year,
             y = value,
             group = pop_type)) +
  geom_point() +
  geom_line() +
  scale_y_log10()


