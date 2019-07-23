library(tidyverse)

# read the data
birds <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

# plot a time series
birds %>%  
  # count incidents by date
  count(incident_date) %>% 
  ggplot(aes(x = incident_date,
             y = n)) +
  geom_line()

