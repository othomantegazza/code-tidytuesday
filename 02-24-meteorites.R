library(tidyverse)
library(grid)
library(maptools)
library(sf)

purple <- "#AA2255"
purple2 <- "#BB2255"

# Get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-06-11/meteorites.csv")

data_path <- "data/2-24-meteorites.Rdata"

if(!file.exists(data_path)) {
  meteors <- 
    data_url %>% 
    read_csv()
  
  save(meteors, file = data_path)
} else {
  load(data_path)
}


# how many by years? ------------------------------------------------------

meteors %>% 
  count(year) %>% 
  arrange(desc(year)) # %>% View()

# there are peak years 2003, 1998, 1988, 1979
# ten years cycle?


meteors %>% 
  filter(fall == "Fell") %>% View()
  count(year) %>% 
  filter(year > 1950,
         year < 2014) %>% 
  ggplot(aes(x = year, 
             y = n)) +
  geom_line()


# where do they fall? -----------------------------------------------------

meteors %>% 
  mutate(long = case_when(long > 200 ~ long - 360,
                          TRUE ~ long)) %>% 
  ggplot(aes(x = long,
             y = lat)) +
  geom_point(alpha = .4) +
  coord_map(projection = "gall", lat0 = 0)

meteors %>% 
  filter(long > 200) %>% View()

# size by year -----------------------------------------------------------

meteors %>% 
  filter(year > 1950,
         year < 2014) %>% 
  ggplot(aes(x = year %>% as.character(),
             y = mass)) +
  # geom_boxplot(fill = "#CD0A62") +
  geom_boxplot(aes(fill = fall), position = "dodge") +
  scale_y_log10() +
  # facet_wrap(facets = "fall", ncol = 1) +
  theme_bw()


# size of fell meteorites by location -------------------------------------

meteors %>% 
  filter(long < 200,
         fall == "Fell",
         mass >= 250000) %>% 
  ggplot() +
  geom_path(data = map_data("world"),
            aes(x = long, y = lat,
                group = group),
            colour = "grey70") +
  # geom_sf(data = wrld_simpl %>%
  #           sf::st_as_sf()) +
  geom_point(aes(x = long,
                 y = lat,
                 size = mass),
             colour = purple) +
  theme_void() +
  coord_map(projection = "mollweide")#, lat0 = 0) 

meteors %>% 
  filter(long < 200,
         fall == "Fell",
         mass >= 250000) %>% 
  ggplot() +
  # geom_path(data = map_data("world"),
  #           aes(x = long, y = lat,
  #               group = group),
  #           colour = "grey70") +
  geom_sf(data = wrld_simpl %>%
            sf::st_as_sf(crs = "+proj=moll +datum=WGS84 +no_defs")) +
  geom_point(aes(x = long,
                 y = lat,
                 size = mass),
             colour = purple) +
  theme_void() +
  coord_sf(crs = "+proj=moll +datum=WGS84 +no_defs")

"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

meteors %>% 
  filter(long < 200,
         fall == "Fell",
         mass >= 250000) %>% 
  ggplot() +
  # geom_path(data = map_data("world"),
  #           aes(x = long, y = lat,
  #               group = group),
  #           colour = "grey70") +
  geom_sf(data = wrld_simpl %>%
            sf::st_as_sf()) +
  geom_point(aes(x = long,
                 y = lat,
                 size = mass),
             colour = purple) +
  theme_void() +
  coord_map(projection = "mollweide")#, lat0 = 0) 

wrld_simpl %>% sf::st_as_sf() %>% 
  ggplot() +
  geom_sf()
