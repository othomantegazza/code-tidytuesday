library(tidyverse)
library(maptools)
library(maps)
library(broom)

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


# Share density in europe? -----------------------------------------

dat <- dat %>%
  filter(confederation == "UEFA") %>%
  arrange(desc(tv_audience_share))


# Europe Map --------------------------------------------------------------

data("wrld_simpl")
dat$country[!dat$country %in% wrld_simpl$NAME]

country_codes <- wrld_simpl@data %>%
  rename(country = "NAME",
         id = "ISO3") %>%
  select(country, id)

dat <- dat %>%
  left_join(country_codes)

uefa <- wrld_simpl[wrld_simpl$NAME %in% dat$country, ]

# plot(uefa)
# https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/

uefa <- tidy(uefa) %>%
  left_join(dat) %>%
  group_by(id) %>%
  mutate(med_long = mean(range(long)),
         med_lat = mean(range(lat))) %>%
  ungroup()

ggplot(data = uefa,
       aes(fill = tv_audience_share,
           x = long,
           y = lat,
           group = group,
           label = tv_audience_share)) +
  geom_polygon(size=0, alpha=0.9) +
  geom_text(aes(x = med_long,
                y = med_lat,
                size = tv_audience_share*2),
            colour = "darkgrey") +
  xlim(-10, 50) +
  ylim(32, 77) +
  theme_void()

