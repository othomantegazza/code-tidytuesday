library(tidyverse)
library(maps)
# library(cowplot)

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

map_plot <- 
  dat_tidy %>%
  ggplot(aes(x = xlong,
             y = ylat
             # colour = t_cap
             )) +
  # geom_point(size = .3, 
  #            alpha = .1) +
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
                                               barwidth = 12,
                                               barheight = .4)) +
  theme_void() +
  theme(strip.text = element_text(size = 12,
                                  colour = "grey40"), 
        title = element_text(colour = "grey20",
                             face = "bold"), 
        legend.position = "bottom", 
        plot.margin = margin(18, 10, 10, 10,
                             unit = "pt")) + 
  labs(title = "New Wind Turbunes in US mainland",
       caption = "Source: ; plot by @othomn")




png(filename = "plots/32-wind-turbines.png",
    height = 3000, width = 1700,
    res = 400)
map_plot %>% print()
dev.off()
# Explore time patterns ---------------------------------------------------

# time_plot <- 
#   dat_tidy %>% 
#   ggplot(aes(x =  p_year,
#              y = t_cap)) +
#   # ggbeeswarm::geom_quasirandom(size = .2,
#   #                              alpha = .2)
#   stat_bin2d(binwidth = c(1, 500),
#              alpha = .7) +
#   scale_fill_viridis_c(
#     option = "E",
#     guide = guide_colourbar(title.vjust = 1,
#                             barwidth = 7,
#                             barheight = .2)
#   ) +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "top") 

# dat %>% filter(t_cap > 5000) %>% pull(p_name)


# Put them together -------------------------------------------------------

# p <- plot_grid(map_plot,
#                time_plot,
#                ncol = 2,
#                rel_widths = c(1.6,1))
# 
# png(filename = "plots/32-wind-turbines.png",
#     height = 2000, width = 2200,
#     res = 300)
# p %>% print()
# dev.off()
