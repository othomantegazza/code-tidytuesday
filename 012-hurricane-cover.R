library(tidyverse)
library(lubridate)

# Read files --------------------------------------------------------------

local_data <- "data/hurrican-cover.Rdata"


if(!file.exists(local_data)) {
  paths <- c(google_trends = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_google_trends.csv",
             hurricanes = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_hurricanes.csv",
             states = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_states.csv",
             top_online_news = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_top_online_news.csv",
             trump = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_trump.csv",
             tv_hurricane = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_tv_hurricanes.csv")
  
  dat <- paths %>%
    map(read_csv)
  
  # the first file complains
  # 
  # Parsed with column specification:
  #   cols(
  #     `Category: All categories` = col_character()
  #   )
  # Warning: 38 parsing failures.
  #
  # read manually
  
  dat$google_trends <- read_csv(paths["google_trends"], skip = 2)
  
  save(dat, file = "data/hurrican-cover.Rdata")
} else {
  load(local_data)
}


# Small tidying -----------------------------------------------------------

dat$hurricanes <- dat$hurricanes %>% mutate(Date = mdy(Date))
dat$states <- dat$states %>% mutate(Date = mdy(Date))
dat$tv_hurricane <- dat$tv_hurricane %>% mutate(Date = mdy(Date))
dat$top_online_news <- NULL
dat$google_trends <- dat$google_trends %>% rename(Date = "Day")

tst <- dat %>% reduce(full_join)
