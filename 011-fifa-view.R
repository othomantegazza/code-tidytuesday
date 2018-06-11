library(tidyverse)


# Get data ----------------------------------------------------------------

dat_path <- "data/fifa.Rdata"
if(!file.exists(dat_path)) {
  dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week11_fifa_audience.csv",
                  col_types = cols_only(country = col_character(),
                                        confederation = col_character(),
                                        population_share = col_double(),
                                        tv_audience_share = col_double(),
                                        gdp_weighted_share = col_double()))
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# Share per country? -----------------------------------------

dat <- dat %>% 
  arrange(desc(tv_audience_share)) %>%
  mutate(rank = 1:nrow(.)) %>%
  filter(rank <= 50)


# Plot rank share by country ----------------------------------------------

ggplot(data = dat,
       aes(x = rank,
           y = tv_audience_share)) + 
  # geom_point(alpha = .01) + 
  geom_linerange(aes(ymin = 0, ymax = tv_audience_share), lwd = 4) + 
  geom_hline(yintercept = 0, lwd = .05) +
  geom_rug(aes(x = rank, y = NULL), alpha = .5) +
  facet_wrap(facets = "confederation", ncol = 1) +
  theme_bw()
