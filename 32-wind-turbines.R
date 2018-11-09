library(tidyverse)
library(maps)

# Get data ----------------------------------------------------------------

dat_path <- paste0("https://raw.githubusercontent.com/",
                   "rfordatascience/tidytuesday/",
                   "master/data/2018-11-06/us_wind.csv")

dat_file <- "data/32-wind-turbine.Rdata"

if(!file.exists(dat_file)) {
  dat <- read_csv(dat_path)
  
  save(dat, file = dat_file)
} else {
  load(dat_file)
}
# Remove NA ---------------------------------------------------------------

dat <- 
  dat %>% 
  select(p_year, p_cap, t_cap,
         xlong, ylat) %>%
  mutate(p_year = p_year %>%
           na_if(-9999),
         p_cap = p_cap %>%
           na_if(-9999),
         t_cap = t_cap %>% 
           na_if(-9999))
         
dat_tidy <-
  dat %>% 
  filter(complete.cases(.)) %>% 
  mutate(year_span = case_when(p_year >= 1981 & p_year <= 1993 ~ "1981 - 1993",
                               p_year >= 1994 & p_year <= 2006 ~ "1994 - 2006",
                               p_year >= 2007 & p_year <= 2018 ~ "2007 - 2018"))

# explore geographical patterns--------------------------------------------------------

usa_map <- map_data("state")

dat_tidy %>%
  ggplot(aes(x = xlong,
             y = ylat,
             colour = t_cap)) +
  geom_point(size = .5, 
             alpha = .3) +
  geom_map(data=usa_map,
           map=usa_map,
           aes(x=long, y=lat,
               map_id=region),
           colour = "grey30", fill = NA,
           size = .2) +
  facet_grid(. ~ year_span) +
  coord_map(projection = "conic", lat = 40) +
  lims(x = c(-119, -74),
       y = c(25, 50)) +
  scale_color_viridis_c(guide = guide_colourbar(title.vjust = .7,
                                                barwidth = 20,
                                                barheight = .3)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Wind Turbine Installed in the US")


# Explore time patterns ---------------------------------------------------

library(ggridges)

dat_tidy %>% 
  ggplot(aes(x = t_cap,
             y = p_year %>%
               as.character() %>% 
               as.factor() %>%
               fct_rev())) +
  # ggbeeswarm::geom_quasirandom() +
  # scale_colour_viridis_c() 
  geom_density_ridges(panel_scaling = FALSE)

dat_tidy %>% 
  ggplot(aes(x =  p_year,
             y = t_cap)) +
  # ggbeeswarm::geom_quasirandom(size = .2,
  #                              alpha = .2)
  stat_bin2d(binwidth = c(1, 500)) +
  scale_fill_viridis_c() +
  theme_bw()
# dat %>% filter(t_cap > 5000) %>% pull(p_name)
