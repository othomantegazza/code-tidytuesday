library(tidyverse)
library(lubridate)

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

ufo %>%
  ggplot(aes(x = longitude,
                   y = latitude)) +
  geom_hex(colour = "grey", size = .2, bins = c(100, 120)) +
  lims(x = c(-120, -50),
       y = c(25, 50))
