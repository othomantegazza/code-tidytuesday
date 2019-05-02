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
    ungroup() #%>% 
    # mutate(light_score = light_score %>% as.character() %>% as.factor())
  
  # fit <- glm(n ~ light_score - 1, data = to_fit, family = "poisson") #%>% summary()
  fit <- glm(n ~ light_score, data = to_fit, family = "poisson") #%>% summary()
  
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

p_top4 <- 
  dat %>% 
  filter(species %in% top4) %>% 
  group_by(species, date, light_score, flight_call) %>%
  count() %>% # pull(light_score) %>% table()
  ggplot(aes(x = light_score,
             y = n,
             # fill = "#3752C3",
             group = light_score)) +
  ggbeeswarm::geom_quasirandom(groupOnX = T,
                               alpha = .7,
                               size = .3,
                               fill = "#3752C3",
                               shape = 21)  +
  facet_wrap(facets = "species") +
  # scale_y_continuous(trans = "log") +
  # scale_fill_viridis_c() +
  theme_bw() +
  theme(text = element_text(color = "grey5"),
        strip.text = element_text(face = "italic")) +
  labs(x = "Windows Light Score",
       y = "Number of Strikes per Night",
       title = "Do window lights attract birds in collisions with buildings?",
       subtitle = str_wrap("Top 4 species that might be sensitive to building's window lights,
                    data from Winger et al., (2019).", width = 110),
       caption = paste("plot by @othomn",
                       str_wrap("Source: Winger BM, Weeks BC, Farnsworth A, Jones AW, Hennen M,
                                Willard DE (2019) Nocturnal flight-calling behaviour predicts
                                vulnerability to artificial light in migratory birds. Proceedings
                                of the Royal Society B 286(1900):
                                20190364. https://doi.org/10.1098/rspb.2019.0364",
                                width = 120),
                       sep = "\n"))

png(filename = "plots/2-18-bird-collisions.png",
    height = 7.5,
    width = 7,
    res = 300,
    units = "in")
p_top4
dev.off()


# lineplot all species ----------------------------------------------------

p_lines <- 
  dat %>% 
  mutate(species = species %>% factor(levels = loglik_df$species)) %>% 
  group_by(species, date, light_score, flight_call) %>%
  count() %>% # pull(light_score) %>% table()
  group_by(species, light_score, flight_call) %>%
  summarise(n_mean = mean(n)) %>% 
  ggplot(aes(x = light_score,
             y = n_mean,
             colour = flight_call)) +
  geom_point(shape = 21) +
  geom_line(size = 4)  +
  facet_wrap(facets = "species", ncol = 12) +
  # scale_y_continuous(trans = "log") +
  # scale_fill_viridis_c() +
  theme_bw() +
  theme(text = element_text(color = "grey5"),
        strip.text = element_text(face = "italic")) +
  labs(x = "Windows Light Score",
       y = "Number of Strikes per Night",
       title = "Do window lights attract birds in collisions with buildings?",
       subtitle = str_wrap("Top 4 species that might be sensitive to building's window lights,
                    data from Winger et al., (2019).", width = 110),
       caption = paste("plot by @othomn",
                       str_wrap("Source: Winger BM, Weeks BC, Farnsworth A, Jones AW, Hennen M,
                                Willard DE (2019) Nocturnal flight-calling behaviour predicts
                                vulnerability to artificial light in migratory birds. Proceedings
                                of the Royal Society B 286(1900):
                                20190364. https://doi.org/10.1098/rspb.2019.0364",
                                width = 120),
                       sep = "\n"))


png(filename = "plots/2-18-bird-collisions-all.png",
    height = 15,
    width = 15,
    res = 300,
    units = "in")
p_lines
dev.off()
