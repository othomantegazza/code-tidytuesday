library(tidyverse) # A collection of packages for Data Science
library(lubridate) # use dates
library(gganimate)

# IN YOUR WORKING DIRECTORY, MAKE A FOLDER NAMED "data" 
# TO STORE YOUR DATA

# read data ---------------------------------------------------------------

# and save them locally as .Rdata

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-07-23/wildlife_impacts.csv")

data_path <- "data/2-30-bird-strikes.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  birds <- 
    data_url %>% 
    read_csv() %>% 
    # inconsistently capitalized
    mutate(phase_of_flt = tolower(phase_of_flt),
           # extract minutes
           minutes = time %% 100,
           # extract hours
           hours = time %/% 100) %>% 
    # One record has negative time
    filter(time >= 0)
  
  save(birds, file = data_path)
} else {
  load(data_path)
}

# second animation --------------------------------------------------------

p2 <- birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  count(incident_month, at_hours) %>% 
  group_by(at_hours) %>%
  mutate(med_n = mean(n)) %>%
  ungroup() %>%
  ggplot(aes(x = incident_month)) +
  geom_segment(aes(xend = incident_month,
                   yend = n,
                   y = med_n),
               linetype = 2) +
  geom_segment(data = . %>% group_by(at_hours) %>% 
                 summarise(med_n = mean(n)),
               aes(y = med_n,
                   yend = med_n,
                   x = 0,
                   xend = 12)) +
  # geom_point(aes(y = n), shape = 1, size = 10) +
  geom_point(aes(y = n, colour = n, size = n)) +
  guides(size = FALSE,
         colour = FALSE) +
  labs(x = "",
       y = "Number of Accidents",
       title = "At {closest_state}") +
  scale_y_continuous(limits = c(-100, 500), expand = c(0, 0)) +
  # coord_polar() +
  theme_void() +
  # theme(aspect.ratio = .7,
  #       panel.background = element_rect(colour = "black"),
  #       panel.grid = element_blank()) +
  # facet_wrap(facets = "at_hours")
  transition_states(at_hours) +
  ease_aes('cubic-in-out')

p2
# animate(p2, nframes = 300, fps = 15)
