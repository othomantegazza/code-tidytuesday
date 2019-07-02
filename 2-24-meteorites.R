library(tidyverse)
library(grid)
library(maptools)
library(sf)

purple <- "#AA2255"
purple2 <- "#BB2255"
bg_col <- "#EAEA9F" #"#F6F6DF"

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
  filter(fall == "Fell") %>% # View()
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
  filter(long > 200) # %>% View()

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

p <- 
  meteors %>% 
  filter(long < 200,
         fall == "Fell",
         mass >= 250000) %>% # View()
  ggplot(aes(x = long,
             y = lat)) +
  # geom_path(data = map_data("world"),
  #           aes(x = long, y = lat,
  #               group = group),
  #           colour = "grey70") +
  borders(fill = "white", colour = bg_col) +
  # geom_sf(data = wrld_simpl %>%
  #           sf::st_as_sf()) +
  ggrepel::geom_text_repel(aes(label = paste(name,
                                             scales::scientific(mass, digits = 0),
                                             sep = "\n")),
                           force = 8,
                           lineheight = .8, 
                           colour = purple,
                           size = 3,
                           fontface = "bold") +
  geom_point(aes(size = mass),
             colour = "#4C63C3",
             alpha = .7) +
  guides(size = FALSE,
         colour = FALSE) +
  scale_colour_viridis_c(option = "A") +
  theme_void() +
  theme(panel.background =  element_rect(fill = bg_col, colour = bg_col),
        title = element_text(hjust = .5)) +
  coord_map(projection = "mollweide", orientation = c(90, 0, 0))#, lat0 = 0) 

grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
print(p, vp = viewport())


png("plots/2-24-meteorites.png",
    width = 2500,
    height = 1800,
    res = 300)
set.seed(46)
grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
print(p, vp = viewport(y = .45))
grid.text(label = str_wrap("Biggest meteorites that have been observed hitting Earth until 2012.",
                           width = 50),
          vjust = .5,
          hjust = .5,
          x = .5,
          y = .9, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 14,
                    col = "#7A82A6",
                    lineheight = 1))
grid.text(label = str_wrap("With name and mass in grams.",
                           width = 50),
          vjust = .5,
          hjust = .5,
          x = .5,
          y = .84, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 7,
                    col = purple))
grid.text(label = str_wrap("Data from NASA | plot by @othomn",
                           width = 50),
          vjust = .5,
          hjust = .5,
          x = .78,
          y = .06, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 8,
                    col = "#7A82A6"))
dev.off()


meteors %>% 
  filter(long < 200,
         fall == "Fell",
         mass >= 250000) %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
ggplot() +
  # geom_path(data = map_data("world"),
  #           aes(x = long, y = lat,
  #               group = group),
  #           colour = "grey70") +
  geom_sf(data = wrld_simpl %>%
            sf::st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")) +
  geom_sf(aes(size = mass),
          colour = purple) +
  geom_sf_text(aes(label = name)) +
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
