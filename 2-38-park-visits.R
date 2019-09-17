library(tidyverse)
library(janitor)
library(ggrepel)


# get the data ------------------------------------------------------------

park_visit_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/",
                         "data/2019/2019-09-17/All%20National%20Parks%20Visitation%201904-2016.csv")

# before running this, make a folder named data in your working directory
park_visit_path <- "data/2-38-park-visits.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(park_visit_path)) {
  visits <- 
    park_visit_url %>% 
    read_csv() %>% 
    janitor::clean_names()
  
  save(visits, file = park_visit_path)
} else {
  load(park_visit_path)
}


# tidy -----------------------------------------------------------------

# year_raw is formatted as character
visits %>% 
  pull(year_raw) %>% 
  unique()



# any NA?
visits2 %>% 
  map(~is.na(.) %>% sum())
# the column parkname has a lot of NA and can be removed
# redundant with unit_name ?

# filtering in base R
visits_base <- visits[visits$year_raw != "Total", ]
visits_base$year <- NULL
visits_base$parkname <- NULL
visits_base$year_raw <- as.numeric(visits_base$year_raw)

# tidy year raw and remove year column
# with declarative sintax
visits2 <- 
  visits %>% 
  filter(year_raw != "Total") %>% 
  mutate(year_raw = as.numeric(year_raw)) %>% 
  select(-year, parkname)


# explore -----------------------------------------------------------------

# how many parks
visits2 %>%
  pull(unit_name) %>% 
  unique()

# or
visits2 %>% 
  count(unit_name, sort = TRUE)

# plot --------------------------------------------------------------------


p_box <- 
  visits2 %>% 
  ggplot(aes(x = year_raw,
             y = visitors,
             group = year_raw)) +
  geom_boxplot(fill = "#68DDFF")

p_box

p_box +
  scale_y_log10()

p_points <-
  visits2 %>% 
  ggplot(aes(x = year_raw,
             y = visitors)) +
  theme_bw()

p_points

p_points2 <- 
  p_points +
  geom_point(aes(colour = region),
             alpha = .7)

p_points2

p_points +
  geom_point(alpha = .3, colour = "grey70") +
  geom_smooth(aes(colour = region)) +
  scale_y_log10()


visits_by_region <- 
  visits2 %>% 
  group_by(year_raw, region) %>% 
  summarize(visitors = sum(visitors))

p_bars <- 
  visits_by_region %>% 
  ggplot(aes(x = reorder(region, visitors),
             y = visitors,
             fill = visitors)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c()

p_bars

p_bars_facets <- 
  p_bars +
  facet_wrap(facets = "year_raw")

p_bars_facets

# fix the x labels
p_bars_facets+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = .5))

p_lines <- 
  visits2 %>% 
  ggplot(aes(x = year_raw,
             y = visitors)) +
  geom_line(aes(group = unit_code),
            alpha = .2)
  # scale_y_log10()

p

visits_top <- 
  visits2 %>%
  filter(year_raw == max(year_raw)) %>% 
  filter(visitors > quantile(visitors, .99))
  

p2 <- 
  p + 
  geom_line(data = . %>% 
              filter(unit_code %in% visits_top$unit_code),
            aes(colour = unit_code,
                group = unit_code),
            size = 1) +
  geom_label_repel(data = visits_top,
                  aes(label = unit_name %>% str_wrap(width = 20),
                      colour = unit_code),
                  xlim = c(2020, NA),
                  size = 3) +
  lims(x = c(NA, 2050)) +
  guides(colour = FALSE) +
  theme_minimal()  
 

p2



# on a map ----------------------------------------------------------------

library(sf)
library(geojsonsf)

# read_map_data -----------------------------------------------------------

# park_maps <- geojson_sf("https://opendata.arcgis.com/datasets/6042ea0d29894cc4a694d34b5812b4a1_0.geojson")

map_file <- "data/2-38-park-visits.zip"

download.file(url = "https://opendata.arcgis.com/datasets/6042ea0d29894cc4a694d34b5812b4a1_0.zip",
              destfile = map_file)
map_file <- unzip(map_file, overwrite = T)

read_sf(temp %>% unzip())
