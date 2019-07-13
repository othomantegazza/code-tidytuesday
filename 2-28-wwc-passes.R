library(tidyverse)
library(jsonlite)
library(lubridate)


# get data ----------------------------------------------------------------

pass_url <- paste0("https://raw.githubusercontent.com/statsbomb/",
                   "open-data/master/data/events/69321.json")

pass_path <- "data/2-28-wwc-passes.Rdata"


if(!file.exists(pass_path)) {
  pass <-
    fromJSON(pass_url, flatten = TRUE) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    # time as time
    mutate(timestamp = timestamp %>% str_sub(1, 8),
           timestamp = timestamp %>% hms())
  
  save(pass, file = pass_path)
} else {
  load(pass_path)
}



# clean data --------------------------------------------------------------

# fix location
pass_to_plot <- 
  pass %>%
  filter(type_name == "Pass") %>% 
  mutate(location_x = location %>% map(~eval(.) %>% .[1]) %>% flatten_dbl(),
         location_y = location %>% map(~eval(.) %>% .[2]) %>% flatten_dbl(),
         pass_end_location_x = pass_end_location%>% map(~eval(.) %>% .[1]) %>% flatten_dbl(),
         pass_end_location_y = pass_end_location%>% map(~eval(.) %>% .[2]) %>% flatten_dbl()) %>% 
  select(timestamp, location_x, location_y, pass_end_location_x, pass_end_location_y)

# xy on field
pass_to_plot %>% 
  ggplot(aes(x = location_x,
             y = location_y,
             xend = pass_end_location_x,
             yend = pass_end_location_y)) +
  geom_segment()

# depth vs time
# 
# pass_to_plot %>% 
#   ggplot(aes(x))
