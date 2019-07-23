library(tidyverse) # A collection of packages for Data Science
library(lubridate) # use dates

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
           hours = time %/% 100)
  
  save(birds, file = data_path)
} else {
  load(data_path)
}


birds %>% drop_na(incident_date)

# explore -----------------------------------------------------------------

# Any NAs?
birds %>% map(~is.na(.) %>% sum())

## strikes vs categoricals variables

# All levels of categorical variables
birds %>% 
  select_if(is.character) %>%
  map(unique)

# Count them
birds %>% 
  select_if(is.character) %>%
  map(~tibble(x = .) %>% count(x) %>% arrange(desc(n)))

## Plot timeline: strike intensity change cyclically during the year 
birds %>%  
  count(incident_date) %>% 
  ggplot(aes(x = incident_date,
             y = n)) +
  geom_line()

# Plot timeline, sum of strikes for each day of the year
birds %>% 
  mutate(yday = yday(incident_date)) %>% 
  ggplot(aes(x = yday)) +
  geom_bar() 

# Plot timeline, sum of strikes for each month of the year
birds %>% 
  ggplot(aes(x = incident_month)) +
  geom_bar() 

# Same with fill for time of the day
birds %>% 
  ggplot(aes(x = incident_month,
             fill = time_of_day)) +
  geom_bar() 



# Plot - What happens when the airplane is on the ground ------------------------------------

# Example of plot refined with title and labels

# there might be more designation when airplane is on the ground
on_ground <- c("take-off run", "landing roll", "taxi")

# Top 20 birds when plane is on ground
p <- 
  birds %>% 
  # Prepare the data
  filter(phase_of_flt %in% on_ground) %>% 
  count(species) %>%
  top_n(20, wt = n) %>% 
  # plot
  ggplot(aes(x = reorder(species, n),
             y = n)) +
  geom_bar(stat = "identity", fill = "#4C63C3") +
  # adjust coordinates
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  # title and labels
  labs(x = "Species",
       y = "n",
       title = "Wildlife Strikes with Airplanes during Ground Phases",
       subtitle = str_wrap("Recorded in the US between 1990 and 2018.
                           The Plot shows the top 10 species that are hit during
                           ground phases of taxiing, take off and landing.",
                           width = 60),
       caption = "Data by FAA | Plot by @othomn") +
  # tweak plot appearance
  theme_minimal() +
  theme(text = element_text(colour = "grey15"))

# check plot
p


# save plot ---------------------------------------------------------------
png(filename = "plots/2-30-bird-strikes.png",
    res = 300,
    height = 1800,
    width = 1900)
p %>% print()
dev.off()
  

