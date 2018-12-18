library(tidyverse)
library(glue)

# Get Data ----------------------------------------------------------------


dat_path <- "data/37-restaurant-rate.Rdata"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(paste0("https://raw.githubusercontent.com/",
                    "rfordatascience/tidytuesday/master/data/",
                    "2018-12-11/nyc_restaurants.csv"))
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}
