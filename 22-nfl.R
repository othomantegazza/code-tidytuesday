library(tidyverse)

# Get data ----------------------------------------------------------------


nfl_path <- "data/22-nfl.Rdata"
  
if(!file.exists(nfl_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/",
                         "master/data/2018-08-28/nfl_2010-2017.csv"))
  save(dat, file = nfl_path)
} else {
  load(nfl_path)
}


# Explore data ------------------------------------------------------------

dat %>% 
  group_by(game_year) %>%
  summarise(tot_rush = sum(rush_yds, na.rm = T),
            tot_pass = sum(pass_yds, na.rm = T))
