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


# Check NAs ---------------------------------------------------------------

dat_state <- 
  dat %>% 
  filter(pop_category == "Total",
         year > 1982, 
         year != 2016) %>% 
  group_by(year, state) %>% 
  summarise(prison_population = sum(prison_population, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>% 
  mutate(ratio = prison_population/population)

p <- 
  dat_state %>% 
  ggplot(aes(x = year,
             y = state,
             fill = prison_population)) +
  geom_raster() +
  scale_fill_viridis_c(trans = "log")

png(filename = "plots/2-04-prison-NAs.png",
    height = 1500, width = 1500,
    res = 300)
p %>% print()
dev.off()

dat %>% filter(state == "NM") %>% pull(prison_population) %>% is.na() %>% all()

# Only on counties without na? --------------------------------------------

dat$prison_population %>% is.na() %>% sum()

tst <- 
  dat %>% 
  filter(pop_category == "Total",
         year > 1988,
         year != 2016) %>% 
  group_by(state, county_name) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()

# tst %>%
#   filter(is.na(prison_population)) %>% View()

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

# plot ratio

tst_sum %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(prison_population = sum(prison_population),
            population = sum(population)) %>% 
  mutate(ratio = prison_population/population) %>% 
  ggplot(aes(x = year,
             y = ratio)) +
  geom_line()


# Try with more details ---------------------------------------------------

by_cat <- 
  dat %>% 
  filter(pop_category != "Total",
         year > 1988,
         year != 2016) %>% 
  group_by(state, county_name) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()

by_cat %>% 
  mutate(ratio = prison_population/population) %>% 
  ggplot(aes(x = year,
             y = ratio,
             group = year)) +
  geom_boxplot() +
  facet_grid(pop_category ~ .,
             scales = "free")

# Observation:
# -ratio of males much higher than ratio of women
# -ratio of black higher than any other ratio, expecially asians
# -ratio of latino high only in three states?