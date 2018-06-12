library(tidyverse)
library(maptools)
library(maps)
library(broom)
library(shadowtext)

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


# Get Capitals ------------------------------------------------------------

capitals <- world.cities %>% 
  filter(capital == 1) %>%
  rename(country = "country.etc") %>%
  mutate(country = if_else(country == "UK",
                           true = "United Kingdom",
                           false = country)) %>%
  right_join(dat) %>%
  filter(complete.cases(.))
 
# plot(uefa)
# https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/

uefa <- tidy(uefa) %>%
  left_join(dat) %>%
  group_by(id) %>%
  mutate(med_long = mean(range(long)),
         med_lat = mean(range(lat))) %>%
  ungroup()


svg(filename = "plots/uefa.svg",
    width = 9, 
    height = 9)
ggplot() +
  geom_polygon(data = uefa,
               aes(fill = tv_audience_share,
                   x = long,
                   y = lat,
                   group = group),
               size=1) +
  geom_shadowtext(data = capitals,
                  aes(x = long,
                      y = lat,
                      label = tv_audience_share,
                      size = tv_audience_share)) +
  xlim(-10, 50) +
  ylim(32, 77) +
  annotate("text", x = -10, y = 74,
          label =  "TV audience share of FIFA World Cup, 2010", size = 6) +
  annotate("text", x = -10, y = 72,
           label =  "UEFA Countries", size = 5,
           hjust = 1.2) +
  coord_map(projection = "conic", lat0 = 50) +
  theme_void() +
  scale_fill_gradient(name="TV Audience Share",
                     breaks=c(0, 1, 2, 3),
                     guide = guide_legend(keyheight = unit(3, units = "mm"),
                                          keywidth=unit(12, units = "mm"), 
                                          label.position = "bottom",
                                          title.position = 'top',
                                          nrow=1),
                     high = "#001a33", low = "#cce6ff") +
  scale_size(guide = FALSE,
             range = c(0, 10)) +
  theme(text = element_text(size = 14),
        legend.position = c(.5, .05))
dev.off()
