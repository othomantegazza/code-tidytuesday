library(tidyverse)

# Get Data ----------------------------------------------------------------


dat_path <- "data/2-04-prison.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "/2019/2019-01-22/prison_population.csv")
# dat_url <- "~/Desktop/tidytuesday/data/2019/2019-01-22/prison_population.csv"


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

dat_clean <- 
  dat %>% 
  filter(pop_category == "Total",
         year >= 1990,
         year != 2016) %>% 
  group_by(state, county_name) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()

# tst %>%
#   filter(is.na(prison_population)) %>% View()

pop_sum <- 
  dat_clean %>% 
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

pop_sum %>% 
  ggplot(aes(x = year,
             y = state,
             fill = ratio)) +
  geom_raster() +
  scale_fill_viridis_c()

dat_clean %>% 
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

pop_sum %>% 
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
  filter(
    # pop_category != "Total",
    year >= 1990,
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

by_cat %>% 
  filter(region == "Midwest") %>%
  {table(.$county_name, .$pop_category)}

by_cat_sum <- 
  by_cat %>% 
  filter(pop_category != "Other") %>% 
  group_by(pop_category, year) %>% 
  summarise(population = sum(population),
            prison_population = sum(prison_population)) %>% 
  ungroup()

View(by_cat_sum)


# Use hypergeometric distribution to discuss category representati --------

# are years and category balanced?

by_cat_sum$pop_category %>% table() # yes, 26 years for each category!

# prepare table for hyopergeometric test:
# get category total next to each other category

by_cat_tot <- 
  by_cat_sum %>% 
  filter(pop_category == "Total") %>% 
  rename_all(funs(paste0(., "_total")))

by_cat_hyp <- 
  by_cat_sum %>% 
  left_join(by_cat_tot, by = c("year" = "year_total"))

# set a function for hypergeometric test

# phyper()
# dhyper()
# rhyper()
# qhyper()

# apply dhyper using pmap

# Define phyper wrapper that contains "..."
# So that it can be used in pmap with extra variables
# Test enrichment
# inspired from
# https://github.com/GuangchuangYu/DOSE/blob/master/R/enricher_internal.R

dhyper2 <- function(x, m, n, k, ...) dhyper(x, m, n, k, log = TRUE)
phyper2 <- function(q, m, n, k, ...) phyper(q, m, n, k, lower.tail = FALSE)


by_cat_hyp2 <- 
  by_cat_hyp %>% 
  # rename arguments for dhyper
  transmute(year = year,
            pop_category = pop_category,
            q = prison_population, # white balls drawn
            x = prison_population, # white balls drawn
            m = population, # white balls in the urn
            n = population_total - population, # black balls in the urn
            k = prison_population_total) %>% # balls drawn from the urn
  # apply dhyper() to every row
  mutate(d = pmap(., .f = dhyper2) %>% purrr::flatten_dbl(),
         p = pmap(., .f = phyper2) %>% purrr::flatten_dbl())

by_cat_hyp2$d
by_cat_hyp2$p

by_cat_hyp2 %>% 
  ggplot(aes(x = year,
             y = -d)) +
  geom_bar(stat = "identity") +
  facet_grid(pop_category ~ .)
