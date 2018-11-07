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

# explore patterns --------------------------------------------------------

# REMEMBER TO REMOVE NA!!!!

usa_map <- map_data("state")

dat %>%
  ggplot(aes(x = xlong,
             y = ylat,
             colour = p_cap)) +
  geom_point(size = .2) +
  geom_map(data=usa_map, map=usa_map,
           aes(x=long, y=lat, map_id=region),
           colour = "grey30", fill = NA, size = .2) +
  coord_map(projection = "conic", lat = 40) +
  lims(x = c(-125, -70),
       y = c(25, 50)) +
  theme_minimal()
