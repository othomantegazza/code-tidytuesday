library(tidyverse) # A collection of packages for Data Science
library(lubridate) # use dates
library(gganimate)

# months in english
Sys.setlocale("LC_TIME", "en_US.UTF8")

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

# explore -----------------------------------------------------------------

birds %>%
  select(time, minutes, hours, time_of_day) %>% 
  summary()

birds %>% 
  filter(hours == -1) # %>% View()

birds %>% 
  select(hours, time_of_day) %>% 
  drop_na() %>% 
  table()

# plot strikes by months / time of day ------------------------------------

birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>%
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         hours = paste(hours, "h") %>% as_factor()) %>% 
  ggplot(aes(x = incident_month)) +
  geom_bar(fill = "#100089", alpha = .6) +
  facet_wrap(facets = "hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5))

# other way around
birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  ggplot(aes(x = hours)) +
  geom_bar(fill = "#100089", alpha = .6) +
  facet_wrap(facets = "incident_month") +
  theme_minimal()


birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) # %>% View()

# animate -----------------------------------------------------------------

p <- birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  ggplot(aes(x = incident_month)) +
  geom_bar(fill = "#100089", alpha = .6) +
  theme_minimal() +
  ggtitle("At {closest_state}") +
  # animations
  transition_states(at_hours) +
  ease_aes('cubic-in-out')
  

pani <- animate(p, nframes = 300, fps = 15)

anim_save("plots/2-30-birds-strikes-animate.gif", animation = pani)


# second animation --------------------------------------------------------

p2 <- birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  count(incident_month, at_hours) %>% 
  ggplot(aes(x = incident_month)) +
  geom_segment(aes(xend = incident_month,
                   yend = n - 10,
                   y = 0),
               linetype = 2) +
  geom_point(aes(y = n), shape = 1, size = 10) +
  geom_point(aes(y = n, colour = n, size = n)) +
  transition_states(at_hours) +
  ease_aes('cubic-in-out') +
  guides(size = FALSE,
         colour = FALSE) +
  labs(x = "",
       y = "Number of Accidents",
       title = "At {closest_state}") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
  theme(aspect.ratio = .7,
        panel.background = element_rect(colour = "black"),
        panel.grid = element_blank())


animate(p2, nframes = 300, fps = 15)



# with smooth -------------------------------------------------------------

p3 <-
  birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  count(incident_month, at_hours) %>%
  rbind(tibble(incident_month = "Feb",
               at_hours = "2 h",
               n = 0)) %>% 
  ggplot(aes(x = incident_month,
             y = n)) +
  geom_point(colour = "#CD0A39") +
  geom_smooth(aes(x = as.numeric(incident_month)),
              span = .8,
              level = .8,
              fill = "grey70",
              colour = "grey70",
              alpha = 1) +
  geom_point(aes(size = n),
             colour = "#CD0A39") +
  # facet_wrap(facets = "at_hours") +
  transition_states(at_hours) +
  ease_aes('cubic-in-out') +
  guides(size = FALSE,
         colour = FALSE) +
  labs(x = "",
       y = "Number of Accidents",
       title = "At {closest_state}") +
  theme_minimal() +
  scale_y_continuous(limits = c(-200, 500), expand = c(0, 0)) +
  theme(aspect.ratio = .7,
        panel.background = element_rect(colour = "black"),
        panel.grid = element_blank())

p3

# other exploratory plots -------------------------------------------------



# with lines?
p_lines <- birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  count(incident_month, at_hours) %>% 
  ggplot(aes(x = incident_month, 
             y = n,
             group = at_hours)) +
  geom_line(colour = "#100089", alpha = .6) +
  geom_point() +
  theme_minimal() +
  ggtitle("At {closest_state}") +
  # animations
  transition_states(at_hours) +
  ease_aes('cubic-in-out')


animate(p_lines, nframes = 300, fps = 15)


# which species -----------------------------------------------------------

birds_month <- 
  birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  filter(!species %>% str_detect("Unknown")) 
  # {table(.$species, .$incident_month)}

top_s <- 
  birds_month %>% 
  count(species) %>% arrange(desc(n)) %>% 
  slice(1:15) %>% pull(species)

birds_month %>% 
  filter(species %in% top_s) %>% 
  ggplot(aes(x = incident_month,
             fill = species)) +
  geom_bar(alpha = .6) +
  facet_wrap(facets = "at_hours") +
  theme_minimal() 


# which species - clustering ----------------------------------------------

top_s <- 
  birds_month %>% 
  count(species) %>% arrange(desc(n)) %>% 
  filter(n >= 30) %>% pull(species)

birds_to_cluster <- 
  birds %>% 
  # most of time measurements are missing
  drop_na(incident_month, time) %>% 
  arrange(hours) %>% 
  mutate(incident_month = incident_month %>% month(label = TRUE),
         at_hours = paste(hours, "h") %>% as_factor()) %>% 
  count(species, incident_month) %>% 
  # scale to 1 - 10
  group_by(species) %>% 
  mutate(n = as.double(n),
         n = scales::rescale(n, to = c(1, 10), from = range(n))) %>% 
  ungroup() %>% 
  spread(incident_month, n) %>% 
  filter(species %in% top_s) %>% 
  # impute NA to 0
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  column_to_rownames("species")

superheat::superheat(birds_to_cluster, 
                     pretty.order.rows = TRUE)
