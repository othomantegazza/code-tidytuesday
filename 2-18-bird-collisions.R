library(tidyverse)
library(lubridate)

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                                   "master/data/2019/2019-04-30/bird_collisions.csv")
data_url2 <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                    "master/data/2019/2019-04-30/mp_light.csv")
data_file <- "data/2-18-bird-collisions.Rdata"


if(!file.exists(data_file)) {
  birds <- readr::read_csv(data_url)
  lights <- readr::read_csv(data_url2)
  
  save(birds, lights, file = data_file)
  
} else {
  load(data_file)
}

# explore -----------------------------------------------------------------

birds$family %>% unique()


birds %>% 
  mutate(year = year(date)) %>% 
  group_by(year, family, locality) %>% 
  count() %>% 
  ggplot(aes(x = year, 
             y = n,
             colour = family,
             group = family)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  facet_grid(. ~ locality)

lights %>% 
  filter(year(date) == 2015) %>% 
  ggplot(aes(x = date,
           y = light_score)) +
  geom_point() +
  geom_line()


# with light score? -------------------------------------------------------

dat <- 
  birds %>% 
  filter(locality == "MP") %>% 
  left_join(lights, by = "date") %>% 
  filter(complete.cases(.))

# no pattern?
dat %>% 
  group_by(date, species, flight_call, light_score) %>%
  count() %>% 
  group_by(species, flight_call, light_score) %>% 
  summarise(mean_strikes = mean(n)) %>% 
  ggplot(aes(x = light_score,
             y = mean_strikes,
             group = light_score)) +
  # geom_point() +
  geom_boxplot() +
  scale_y_log10() +
  facet_grid(. ~ flight_call)

dat %>% 
  group_by(date, species, flight_call, light_score) %>%
  count() %>% 
  group_by(species, flight_call, light_score) %>% 
  summarise(mean_strikes = mean(n)) %>% 
  ggplot(aes(x = light_score,
             y = mean_strikes,
             group = species)) +
  geom_point() +
  # geom_boxplot() +
  geom_line() +
  scale_y_log10() +
  facet_grid(. ~ flight_call)

dat %>% 
  group_by(date, species, flight_call, light_score) %>%
  count() %>% 
  group_by(species, flight_call, light_score) %>% 
  summarise(mean_strikes = mean(n)) %>% 
  group_by(flight_call, light_score) %>% 
  summarise(mean_strikes = mean(mean_strikes)) %>% 
  ggplot(aes(x = light_score,
             y = mean_strikes,
             colour = flight_call)) +
  geom_point() +
  # geom_boxplot() +
  geom_line() +
  scale_y_sqrt()
  # facet_grid(. ~ flight_call)
