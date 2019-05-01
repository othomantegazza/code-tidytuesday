library(tidyverse)
library(lubridate)
library(broom)

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                                   "master/data/2019/2019-04-30/bird_collisions.csv")
data_url2 <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                    "master/data/2019/2019-04-30/mp_light.csv")
# raw_strikes_url <- paste0("https://github.com/rfordatascience/",
#                           "tidytuesday/blob/master/data/2019/",
#                           "2019-04-30/raw/Chicago_collision_data.csv?raw=true")
# raw_lights_url <- paste0("https://raw.githubusercontent.com/",
#                          "rfordatascience/tidytuesday/master/",
#                          "data/2019/2019-04-30/raw/Light_levels_dryad.csv")
data_file <- "data/2-18-bird-collisions.Rdata"


if(!file.exists(data_file)) {s
  birds <- readr::read_csv(data_url)
  lights <- readr::read_csv(data_url2)
  # birds_raw <- readr::read_csv(raw_strikes_url)
  # lights_raw <- readr::read_csv(raw_lights_url)
  
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
  filter(complete.cases(.)) %>% 
  mutate(species = paste(genus, species))

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
  filter(flight_call != "Rare") %>% 
  group_by(date, species, flight_call, light_score) %>%
  count() %>% 
  group_by(species, flight_call, light_score) %>% 
  summarise(mean_strikes = mean(n)) %>% 
  # group_by(flight_call, light_score) %>% 
  # summarise(mean_strikes = mean(mean_strikes)) %>% 
  ggplot(aes(x = light_score,
             y = mean_strikes,
             colour = flight_call)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(facets = "species")

birds %>% 
  select(genus, species) %>% 
  distinct()


# plot all - lm -----------------------------------------------------------

p <- 
  dat %>% 
  group_by(date, species, flight_call, light_score) %>%
  count() %>% 
  ggplot(aes(x = light_score,
         y = n)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  stat_bin_2d(binwidth = 1) +
  facet_wrap(facets = "species", scales = "free_y")


png(filename = "plots/2-18-bird-collisions.png",
    height = 20,
    width = 40,
    res = 300,
    units = "in")
p
dev.off()

# not sure....

p2 <- 
  dat %>% 
  group_by(date, light_score) %>%
  count() %>% # pull(light_score) %>% table()
  ggplot(aes(x = light_score,
             y = n,
             group = light_score)) +
  ggbeeswarm::geom_quasirandom(alpha = .1) 

# tricky tricky dataset, is it a multiple poisson?
p2 + scale_y_continuous(trans = "log")

p3 <- 
  dat %>% 
  group_by(species, date, light_score, flight_call) %>%
  count() %>% # pull(light_score) %>% table()
  ggplot(aes(x = light_score,
             y = n,
             group = light_score,
             colour = flight_call)) +
  ggbeeswarm::geom_quasirandom()  +
  facet_wrap(facets = "species", scales = "free_y")


png(filename = "plots/2-18-bird-collisions-exploratory2.png",
    height = 20,
    width = 40,
    res = 300,
    units = "in")
p3
dev.off()


# poisson model -----------------------------------------------------------

dat$species %>% unique()


get_loglik <- function(dat_spec) {
  # print(dat_spec)
  if(nrow(dat_spec) < 2) return(NA_real_)
  to_fit <- 
    dat_spec %>% 
    group_by(date, light_score) %>%
    count() %>% 
    ungroup() %>% 
    mutate(light_score = light_score %>% as.character() %>% as.factor())
  
  fit <- glm(n ~ light_score - 1, data = to_fit, family = "poisson") #%>% summary()
  
  glance(fit) %>% pull(logLik) %>% as.numeric()
  
}

loglik_df <- 
  dat %>% 
  split(.$species) %>% 
  map(get_loglik) %>% 
  {tibble(species = names(.),
          loglik = flatten_dbl(.))} %>% 
  arrange(loglik)
