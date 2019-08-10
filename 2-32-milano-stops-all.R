library(tidyverse)
library(jsonlite)
library(ggvoronoi)
library(sf)

# milan_shape <- read_sf("data/ds61_infogeo_nil_localizzazione_/NIL OD")
milan_shape <- read_sf("data/comunne_milano/A090101_ComuneMilano.shp")


plot(milan_shape)
# milan_shape %>% as("Spatial")
# milan_shape %>% 
#   ggplot()
#   geom_sf()
# 
# milan_shape$geometry %>% as_data_frame()

outline_milan <- 
  st_transform(select(milan_shape, geometry), crs =  4326) %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  select(lon = "X",
         lat = "Y") %>% 
  as.data.frame()


dat_list <- 
  fromJSON("data/tpl_fermate.geojson")

duomo <- c(lat = 45.464167, lon = 9.191389)

dat <- 
  dat_list%>% 
  .$features %>% 
  {bind_cols(.$properties, .$geometry)} %>% 
  mutate(lon = coordinates %>% map_dbl(~.[1]),
         lat = coordinates %>% map_dbl(~.[2])) %>% 
  sample_n(400) %>% # REMOVE!!!!! JUST TO REDUCE SIZE FOR TESTING
  select(-coordinates) %>% 
  mutate(dist_from_duomo = sqrt((lat - duomo["lat"])^2 + (lon - duomo["lon"])^2)) 
 
# filter points in milano -----------------------------------------------

tst <- st_within(dat %>% st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(value = 4326),
                 milan_shape %>% st_transform(crs = 4326),
                 sparse = FALSE)

# tst_points <- dat %>% st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(value = 4326)
# tst_milan <- milan_shape %>% st_transform(crs = 4326)

# plot(tst_points)
# plot(tst_milan)

dat_in <- 
  dat %>% 
  filter(tst)

# tests -------------------------------------------------------------------

# sel_sgbp = st_intersects(x = nz_height, y = canterbury)


# plot --------------------------------------------------------------------

dat_in %>% 
  ggplot(aes(x = lon,
             y = lat)) +
  geom_voronoi(aes(fill = dist_from_duomo),
               colour = "white",
               size = 0,
               outline = outline_milan) +
  geom_point(size = .2, colour = "grey80") +
  ggforce::geom_delaunay_segment(linetype = 2,
                                 size = .2,
                                 colour = "grey80") +
  scale_fill_viridis_c(end = .7,
                       direction = -1,
                       option = "B",
                       guide = FALSE) +
  theme_void() +
  theme(aspect.ratio = .85)



# with ggforce ------------------------------------------------------------

library(ggforce)

dat_in %>% 
  ggplot(aes(x = lon,
             y = lat)) +
  ggforce::geom_voronoi_tile(aes(fill = dist_from_duomo),
                             colour = "grey90",
                             size = 1.2, bound = outline_milan %>% 
                               rename(x = lon,
                                      y = lat)) +
  ggforce::geom_delaunay_segment(colour = "grey90",
                                 size = .2) +
  geom_point(colour = "grey90") +
  scale_fill_viridis_c(end = .7,
                       direction = -1,
                       option = "B",
                       guide = FALSE) +
  theme(aspect.ratio = .85)
