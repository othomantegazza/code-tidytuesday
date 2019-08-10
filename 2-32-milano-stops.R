library(tidyverse)
library(jsonlite)
library(ggvoronoi)

dat_list <- 
  fromJSON("data/tpl_metrofermate.geojson")

dat <- 
  dat_list%>% 
  .$features %>% 
  {bind_cols(.$properties, .$geometry)} %>% 
  mutate(lon = coordinates %>% map_dbl(~.[1]),
               lat = coordinates %>% map_dbl(~.[2])) %>% 
  select(-coordinates)

dat %>% 
  ggplot(aes(x = lon,
             y = lat)) +
  geom_voronoi(aes(fill = linee),
               colour = "white",
               size = 1) +
  coord_fixed() +
  theme_void()
