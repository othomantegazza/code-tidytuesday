library(tidyverse)
library(lubridate)
library(broom)

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                                   "master/data/2019/2019-04-30/bird_collisions.csv")
data_url2 <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                    "master/data/2019/2019-04-30/mp_light.csv")

data_file <- "data/2-18-bird-collisions.Rdata"


if(!file.exists(data_file)) {s
  birds <- readr::read_csv(data_url)
  lights <- readr::read_csv(data_url2)
  
  save(birds, lights, file = data_file)
  
} else {
  load(data_file)
}

# with light score? -------------------------------------------------------

dat <- 
  birds %>% 
  filter(locality == "MP") %>% 
  left_join(lights, by = "date") %>% 
  filter(complete.cases(.)) %>% 
  mutate(species = paste(genus, species))

# exploratory plot --------------------------------------------------------

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


png(filename = "plots/2-18-bird-collisions-exploratory.png",
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

# poisson model for each species,
# extract log likelihood
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


# plot top 4 --------------------------------------------------------------

top4 <- 
  loglik_df %>% 
  top_n(4, wt = -loglik) %>% 
  pull(species)

dat %>% 
  filter(species %in% top4) %>% 
  group_by(species, date, light_score, flight_call) %>%
  count() %>% # pull(light_score) %>% table()
  ggplot(aes(x = light_score,
             y = n,
             group = light_score)) +
  ggbeeswarm::geom_quasirandom(groupOnX = T,
                               alpha = .4,
                               size = .5)  +
  facet_wrap(facets = "species") +
  # scale_y_continuous(trans = "log")
  theme_bw()
