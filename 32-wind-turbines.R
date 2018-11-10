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


# Plot on map -------------------------------------------------------------

usa_map <- map_data("state")

map_plot <- 
  dat_tidy %>%
  ggplot(aes(x = xlong,
             y = ylat)) +
  geom_map(data=usa_map,
           map=usa_map,
           aes(x=long, y=lat,
               map_id=region),
           colour = "grey60", fill = "grey96",
           size = .4) +
  geom_bin2d(
    bins = c(80, 50)
    ) +
  facet_wrap(facets = "year_span",
             ncol = 1) +
  coord_map(projection = "conic",
            lat = 41) +
  lims(x = c(-124.5, -67.5),
       y = c(25, 50)) +
  scale_fill_viridis_c(guide = guide_colourbar(breaks = c(1000, 1500),
                                               title.vjust = 1,
                                               barwidth = 10.5,
                                               barheight = .4)) +
  theme_void() +
  theme(strip.text = element_text(size = 12,
                                  colour = "grey40"), 
        title = element_text(colour = "grey20",
                             face = "bold"), 
        legend.position = "bottom", 
        plot.margin = margin(18, 10, 10, 10,
                             unit = "pt")) + 
  labs(title = "New Wind Turbines in US Mainland",
       caption = "Source: usgs.gov | plot by @othomn", 
       fill = "Count")

# Save plot ---------------------------------------------------------------

png(filename = "plots/32-wind-turbines.png",
    height = 3000, width = 1700,
    res = 400)
map_plot %>% print()
dev.off()
