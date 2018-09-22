library(MASS)
library(tidyverse)
library(broom)
library(ggrepel)

# Read Data ---------------------------------------------------------------

dat_path <- "data/25-flights.Rdata"


if(!file.exists(dat_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/master/data/",
                         "2018-09-18/us-airports.csv")) %>%
    select(-X1)
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# Analyze data ------------------------------------------------------------

# Many missing data from 2014 onward ?

dat %>%
  split(.$year) %>%
  # map(~sum(is.na(.$passengers)))
  map(dim)

# Subset - only hubs ------------------------------------------------------

hubs <- dat %>%
  dplyr::select(loc_id, hub_type,
         year, passengers, state) %>%
  spread(key = year, value = passengers) %>%
  filter(complete.cases(.),
         hub_type != "Nonhub") %>%
  mutate(x = log(`2012`),
         y = `2017`/`2012`)

# model -------------------------------------------------------------------

fit <- MASS::rlm(y ~ poly(x, 2), data = hubs)

tidy(fit)

hubs <- predict(fit,
        newdata = hubs,
        interval = "predict") %>%
  as_tibble() %>% 
  bind_cols(hubs)

# plot --------------------------------------------------------------------

hubs %>%
  ggplot(aes(x = `2012`,# + `2017`/ 2,
             y =`2017`/`2012`, 
             ymin = lwr,
             ymax = upr)) +
  geom_point() +
  # geom_point(data = hubs %>% filter(state == "OH"),
  #            colour = "blue") +
  geom_text_repel(data = hubs %>%
                    filter(y > upr | y < lwr),
                  aes(label = loc_id)) +
  stat_smooth(method = "rlm",
              formula = y ~ poly(x, 2),
              se = F) +
  geom_ribbon(alpha = .3,
              fill = "cyan3") +
  scale_x_log10() +
  # scale_y_log10() +
  # facet_wrap(facet = "state") +
  theme_bw()




