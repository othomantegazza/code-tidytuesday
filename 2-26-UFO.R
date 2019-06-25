library(tidyverse)
library(jsonlite)
library(lubridate)
library(grid)
library(ggrepel)
library(ggforce)

purple <- "#AA2255"
purple2 <- "#BB2255"
blue <-  "#4C63C3"
mid <- "#D5A080"
mid2 <- "#E2CD92"
grey <- "grey30"
bg_col <- "#F0F0CB" # "#EAEA9F"

cp <- colorRamp(colors = c(bg_col, purple2))

rgb(cp(.1), maxColorValue = 255)

# read data ---------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

data_path <- "data/2-26-ufo.Rdata"

if(!file.exists(data_path)) {
  ufo <- 
    data_url %>% 
    read_csv(col_types = cols(date_documented = col_datetime(format = "%m/%d/%Y")))
  
  save(ufo, file = data_path)
} else {
  load(data_path)
}

# us towns

us_towns <- fromJSON("https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json")

save(us_towns, file = "data/2-26-ufo-us-towns.Rdata")

# explore -----------------------------------------------------------------

ufo %>% 
  ggplot(aes(x = date_documented)) +
  geom_histogram()


ufo %>% 
  ggplot(aes(x = date_documented %>% month() %>% as.character())) +
  geom_bar()


ufo %>% 
  ggplot(aes(x = encounter_length)) +
  geom_histogram() +
  scale_x_log10()

ufo %>% 
  ggplot(aes(x = date_documented,
             y = encounter_length)) +
  # geom_point() +
  geom_hex(colour = "grey", size = .5) +
  scale_y_log10() +
  theme_bw()


# plot --------------------------------------------------------------------

towns_in <- 
  tibble(city = c("New York", "Los Angeles", "Seattle", "Phoenix"),
         nudge_x = c(4, -4, -4, -2),
         nudge_y = c(-2, -2, -2, -2),
         vjust = case_when(nudge_y > 0 ~ 0, TRUE ~ 1),
         hjust = case_when(nudge_x > 0 ~ 0, TRUE ~ 1)) %>% 
  left_join(us_towns, by = "city")

p <- 
  ufo %>%
  ggplot(aes(x = longitude,
             y = latitude)) +
  geom_hex(colour = bg_col, size = .6, bins = c(100, 150)) +
  geom_link(data = towns_in,
               aes(xend = longitude + nudge_x,
                   yend = latitude + nudge_y,
            colour = case_when(..index.. > .12 & ..index.. < .93 ~ "grey",
                                # ~ "grey",
                               TRUE ~ "none")),
            size = .4) +
  geom_text(data = towns_in,
            aes(label = city,
                x = longitude + nudge_x,
                y = latitude + nudge_y,
                hjust = hjust,
                vjust = vjust),
            colour = grey,
            size = 2.6) +
  lims(x = c(-128, -50),
       y = c(25, 49)) +
  scale_fill_gradient(low = mid2,
                      # trans = "log",
                      high = purple) +
  scale_colour_manual(values = c(grey = grey, none = "#FFFFFF00")) +
  guides(colour = FALSE) +
  theme_void() +
  theme(aspect.ratio = .4) 
 
# grid.rect(gp = gpar(fill = bg_col))
# print(p, vp = viewport())

png(filename = "plots/2-26-ufo.png",
    height = 1200, 
    width = 3000,
    res = 400)
grid.rect(gp = gpar(fill = bg_col))
print(p, vp = viewport())
dev.off()
