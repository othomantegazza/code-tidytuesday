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
  mutate(x = `2012`,
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

png(filename = "plots/25-airports.png",
    height = 1500, width = 2500,
    res = 300)
hubs %>%
  ggplot(aes(x = `2012`,# + `2017`/ 2,
             y =`2017`/`2012`, 
             ymin = lwr,
             ymax = upr)) +
  geom_point(colour = "grey30") +
  # geom_point(data = hubs %>% filter(state == "OH"),
  #            colour = "blue") +
  geom_point(data = hubs %>%
               filter(y > upr | y < lwr),
             aes(colour = state)) +
  geom_text_repel(data = hubs %>%
                    filter(y > upr),
                  aes(label = loc_id), nudge_y = .1) +
  geom_text_repel(data = hubs %>%
                    filter(y < lwr),
                  aes(label = loc_id), nudge_y = -.1) +
  # stat_smooth(method = "rlm",
  #             formula = y ~ poly(x, 2),
  #             se = F) +
  geom_line(aes(y = fit), lwd = 1.5,
            alpha = .5) +
  geom_ribbon(alpha = .3,
              fill = "cyan3") +
  scale_x_log10() +
  scale_y_continuous(breaks = c(.75, 1, 1.25, 1.5, 1.75, 2.0, 2.25),
                     labels = paste(c("Decrease - ", "Same - ", "", "", "", "", "Increase - "),
                     c(.75, 1, 1.25, 1.5, 1.75, 2.0, 2.25))) +
  # scale_y_log10() +
  # facet_wrap(facet = "state") +
  theme_bw() +
  labs(title = "Passenger Increase in US Hubs (Airports)",
       subtitle = "Between 2012 and 2017",
       x = "Yearly Passengers in 2012 [log scale]",
       y = "Passengers Increase in 2017 [2017 / 2012]",
       caption = "Source: faa.gov, plot by @othomn")
dev.off() 



