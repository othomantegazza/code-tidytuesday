library(tidyverse)
library(lubridate)
library(countrycode)

# Get data ----------------------------------------------------------------

managed_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

mismanaged_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                         "master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")


data_path <- "data/2-21-plastic-waste.Rdata"

if(!file.exists(data_path)) {
  waste_ok <- read_csv(managed_url) %>% 
    janitor::clean_names() %>% 
    filter(year == 2010)
  
  waste_lost <- read_csv(mismanaged_url) %>% 
    janitor::clean_names() %>% 
    filter(year == 2010)
  
  save(waste_ok, waste_lost, file = data_path)
} else {
  load(data_path)
}


waste <-
  full_join(waste_ok %>%
              select(entity,
                     code,
                     plastic = per_capita_plastic_waste_kilograms_per_person_per_day),
            waste_lost %>% 
              select(entity,
                     code,
                     lost = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day)) %>% 
  # select(entity, code, year,
  #        ok = per_capita_plastic_waste_kilograms_per_person_per_day,
  #        lost = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day) %>% 
  drop_na() 

# Explore -----------------------------------------------------------------

waste %>% 
  ggplot(aes(x = plastic,
             y = lost)) +
  geom_point() 
  # scale_x_log10() +
  # scale_y_log10()
  # ggrepel::geom_text_repel(aes(label = code))

waste <- 
  waste %>% 
  mutate(ratio_lost = lost/plastic)

# two groups?
waste %>% 
  ggplot(aes(x = ratio_lost)) +
  geom_histogram()

waste %>%
  filter(ratio_lost > .75) %>% 
  pull(entity)

waste %>%
  filter(ratio_lost <.1 ) %>% 
  pull(entity)


# with continent ----------------------------------------------------------

waste_cont <- 
  waste %>% mutate(continent = countrycode(sourcevar = entity,
                                         origin = "country.name",
                                         destination = "continent"),
                 continent = case_when(entity == "Micronesia (country)" ~ "Oceania",
                                    TRUE ~ continent)) %>% 
  drop_na(continent)

waste_cont %>% 
  ggplot(aes(x = ratio_lost,
             fill = continent,
             colour = continent)) +
  geom_histogram(bins = 20, alpha = .5) +
  theme_minimal()

