library(tidyverse)
library(lubridate)

# read data ---------------------------------------------------------------

# and save them locally as .Rdata

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/",
                   "data/2019/2019-07-23/bird_impacts.csv")

data_path <- "data/2-30-bird-strikes.Rdata"


if(!file.exists(data_path)) {
  birds <- 
    data_url %>% 
    read_csv() %>% 
    # inconsistently capitalized
    mutate(phase_of_flt = tolower(phase_of_flt),
           # minutes
           minutes = time %% 100,
           # hours
           hours = time %/% 100)
  
  save(birds, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

# NAs?
birds %>% map(~is.na(.) %>% sum())

# strikes vs categoricals variables
birds %>% 
  select_if(is.character) %>%
  map(unique)

birds %>% 
  select_if(is.character) %>%
  map(~tibble(x = .) %>% count(x) %>% arrange(desc(n)))

# timeline, definitely cyclical, yearly 
birds %>%  
  count(incident_date) %>% 
  ggplot(aes(x = incident_date,
             y = n)) +
  geom_line()

# timeline, year days
birds %>% 
  mutate(yday = yday(incident_date)) %>% 
  ggplot(aes(x = yday)) +
  geom_bar() 

# timeline months
birds %>% 
  ggplot(aes(x = incident_month)) +
  geom_bar() 


birds %>% 
  ggplot(aes(x = incident_month,
             fill = time_of_day)) +
  geom_bar() 



# Plot - What happens when the airplane is on the ground ------------------------------------

# there might be more designation when airplane is on the ground
on_ground <- c("take-off run", "landing roll", "taxi")

# Top 20 birds when plane is on ground
birds %>% 
  filter(phase_of_flt %in% on_ground) %>% 
  count(species) %>%
  top_n(20, wt = n) %>% 
  ggplot(aes(x = reorder(species, n),
             y = n)) +
  geom_bar(stat = "identity", fill = "#4C63C3") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Species",
       y = "n",
       title = "Wildlife Strikes When Plane is on Ground",
       subtitle = str_wrap("Recorded in the US between 1990 and 2018.
                           The Plot shows the top 10 species that are hit during
                           ground phases of taxiing, take off and landing.",
                           width = 70),
       caption = "Data by FAA | Plot by @othomn") +
  theme_minimal()
  
  
