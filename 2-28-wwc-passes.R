library(tidyverse)
library(jsonlite)
library(lubridate)
library(ggforce)
library(scico)

# get data ----------------------------------------------------------------

pass_url <- paste0("https://raw.githubusercontent.com/statsbomb/",
                   "open-data/master/data/events/69321.json")

pass_path <- "data/2-28-wwc-passes.Rdata"


if(!file.exists(pass_path)) {
  pass <-
    fromJSON(pass_url, flatten = TRUE) %>% 
    as_tibble() %>% 
    janitor::clean_names() 
  
  save(pass, file = pass_path)
} else {
  load(pass_path)
}



# clean data --------------------------------------------------------------

# Fix time
pass_tidy <- 
  pass %>% 
  # time as time
  mutate(timestamp = timestamp %>% str_sub(1, 8),
         # timestamp = timestamp %>% fast_strptime(format = "%H:%M:%S", lt = F))
         timestamp = timestamp %>% hms()) %>%
  mutate(.seconds = minute*60 + second)

# length first period
# length_1st <- 
#   pass_tidy %>% 
#   filter(period == 1) %>%
#   pull(.seconds) %>% max() 

# add first period to seconds
# of second period
# pass_tidy <- 
#   pass_tidy %>% 
#   mutate(.seconds = case_when(period == 1 ~ .seconds,
#                               period == 2 ~ .seconds + length_1st))
  


# fix location
pass_pass <- 
  pass_tidy %>%
  filter(type_name == "Pass") %>% 
  mutate(location_x = location %>% map(~eval(.) %>% .[1]) %>% flatten_dbl(),
         location_y = location %>% map(~eval(.) %>% .[2]) %>% flatten_dbl(),
         pass_end_location_x = pass_end_location%>% map(~eval(.) %>% .[1]) %>% flatten_dbl(),
         pass_end_location_y = pass_end_location%>% map(~eval(.) %>% .[2]) %>% flatten_dbl()) 
  # mutate(timestamp = timestamp %>% seconds() %>% as.numeric()) %>% 

# size of field (meters?)
pass_pass$location_x %>% range()
# [1]   2 120
max_x <- pass_pass$location_x %>% max()

pass_pass$location_y %>% range()
# [1]  1 80
max_y <- pass_pass$location_y %>% max()

# fix team position
pass_pass <- 
  pass_pass %>% split(.$possession_team_id)
  
pass_pass$`851` <- 
  pass_pass$`851` %>% 
  mutate(location_x = max_x - location_x,
         location_y = max_y - location_y,
         pass_end_location_x = max_x - pass_end_location_x,
         pass_end_location_y = max_y - pass_end_location_y)

pass_pass <-  
  pass_pass %>% reduce(bind_rows)
# must invert 

# explore -----------------------------------------------------------------

pass_to_plot <- 
  pass_pass %>% 
  select(.seconds, location_x, location_y,
         pass_end_location_x, pass_end_location_y,
         possession_team_id, index)


# xy on field
pass_to_plot %>% 
  filter(possession_team_id ==  1214) %>% 
  ggplot(aes(x = location_x,
             y = location_y,
             xend = pass_end_location_x,
             yend = pass_end_location_y)) +
  geom_segment()

pass_to_plot %>% 
  filter(possession_team_id ==  851) %>% 
  ggplot(aes(x = location_x,
             y = location_y,
             xend = pass_end_location_x,
             yend = pass_end_location_y)) +
  geom_segment()

# depth vs time
p_pass <- 
  pass_to_plot %>%
  ggplot(aes(y = location_x,
             x = index,
             yend = pass_end_location_x,
             xend = index,
             colour = as.character(possession_team_id))) +
  geom_hline(yintercept = c(0, max_x),
             colour = "grey80") +
  geom_segment(size = .5) +
  # scale_color_scico(palette = "imola") +
  scale_color_manual(values = c("#5867A6", "#E97E00")) +
  guides(colour = FALSE) +
  # coord_polar() +
  # expand_limits(y = c(-150, NA)) +
  theme_void()  



# Other events ------------------------------------------------------------

pass_tidy$type_name %>% unique()
# [1] "Starting XI"       "Half Start"        "Pass"              "Ball Receipt*"     "Duel"             
# [6] "Pressure"          "Dispossessed"      "Interception"      "Dribble"           "Ball Recovery"    
# [11] "Clearance"         "Block"             "Dribbled Past"     "Foul Committed"    "Foul Won"         
# [16] "Miscontrol"        "Goal Keeper"       "Shield"            "Shot"              "Injury Stoppage"  
# [21] "Player Off"        "Player On"         "Referee Ball-Drop" "Half End"          "Substitution"     
# [26] "50/50"             "Tactical Shift"

selected_events <- c("Duel", "Pressure", "Dispossessed",  "Interception", "Dribble",  "Ball Recovery",
                     "Clearance", "Block", "Dribbled Past", "Foul Committed", "Foul Won", 
                     "Injury Stoppage", "Player Off", "Player On", "Referee Ball-Drop",  "Substitution")


# plot event --------------------------------------------------------------

pass_tidy %>% 
  filter(type_name == "Duel") %>%
  ggplot(aes(x = index,
             y = 1, 
             colour = team_name)) +
  geom_point(size = .1) +
  scale_color_manual(values = c("#5867A6", "#E97E00")) +
  theme_void() +
  guides(colour = FALSE)

p_pass


# pass heigth -------------------------------------------------------------


pass_tidy %>% pull(pass_height_name) %>% unique()
# "Ground Pass" "High Pass"   "Low Pass"  

pass_height <- 
  pass_tidy %>% 
  drop_na(pass_height_name) %>% # View()
  mutate(pass_outcome_name = case_when(is.na(pass_outcome_name) ~ "Complete",
                                       TRUE ~ pass_outcome_name)) %>% 
  # filter(type_name == "Pass") %>% 
  ggplot(aes(x = 0,
             y = 0,
             xend = pass_length,
             yend = 0,
             # colour = possession_team_name,
             colour = pass_outcome_name)) +
  geom_curve(data = . %>% filter(pass_height_id == 1),
             alpha = .3,
             curvature = -.2) +
  # geom_curve(data = . %>% filter(pass_height_id == 2),
  #            alpha = .3,
  #            curvature = -.5) +
  # geom_curve(data = . %>% filter(pass_height_id == 3),
  #            alpha = .3,
  #            curvature = -1) +
  lims(y = c(0, .25)) +
  theme_minimal()

pass_height
