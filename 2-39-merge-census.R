# the following script is from Jake Kaupp
# https://github.com/jkaupp/tidytuesdays/blob/master/2019/week39/R/analysis.R

library(tidyverse)
library(janitor)
library(tidycensus)
library(glue)
library(here)

school_diversity <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

## Getting the ACS Survey data ----
acs_var <- load_variables(2017, "acs1", cache = TRUE)

race_vars <- filter(acs_var, concept == "RACE") %>% 
  select(name, label) %>% 
  separate(label, c("estimate", "total", "type"), sep = "!!") %>% 
  mutate(type = coalesce(type, total)) %>% 
  select(name, label = type)


acs_race <- readRDS(here("data/acs_race.RDS"))






# Recoding ACS and aggregating data, sadly not easy to determine Hispanic origin ----
# Following methodology from WaPo repo, recoding Native Hawaiian and Pacifici Islander into Asian.
diversity_data <- acs_race %>% 
  left_join(race_vars, by = c("variable" = "name")) %>% 
  mutate(label = case_when(label == "White alone" ~ "White",
                           label == "Black or African American alone" ~ "Black",
                           label == "American Indian and Alaska Native alone" ~ "AIAN",
                           label == "Native Hawaiian and Other Pacific Islander alone" ~ "Asian",
                           label == "Asian alone" ~ "Asian",
                           label == "Two or more races" ~ "Multi",
                           label == "Some other race alone" ~ "Other",
                           TRUE ~ label)) %>% 
  group_by(GEOID, NAME, label) %>% 
  summarize_at(vars(estimate), sum) 


# Using Simpson's Diversity Index instead of max race metrics for diversity----
totals <- diversity_data %>% 
  summarize(total = sum(estimate)*(sum(estimate)-1))

dvs_score <- diversity_data %>% 
  filter(label != "Total") %>% 
  mutate(es_minus = estimate-1) %>% 
  summarize(numerator = sum(estimate*es_minus)) %>% 
  left_join(totals) %>% 
  mutate(diversity = 1 - numerator/total) %>% 
  select(GEOID, NAME, diversity) 

acs_diversity <- diversity_data %>% 
  spread(label, estimate) %>% 
  select(-Total) %>% 
  left_join(dvs_score) %>% 
  rename(acs_diversity = diversity) %>% 
  ungroup() %>% 
  mutate(NAME = tolower(NAME),
         NAME = str_remove(NAME, "\\(.+\\)"),
         NAME = str_replace_all(NAME, ";", ",")) %>% 
  separate(NAME, c("NAME", "state"), sep = ",") %>% 
  mutate(NAME = str_remove_all(NAME, "school district*+")) %>% 
  select(GEOID, acs_diversity)

# Use WaPo data and calculate Simpson's Diversity Index
upd_school <- school_diversity %>% 
  filter(SCHOOL_YEAR == "2016-2017") %>% 
  select(LEAID, LEA_NAME, ST, SCHOOL_YEAR, AIAN:Total) %>% 
  pivot_longer(AIAN:Multi, "race", "value") %>% 
  mutate(n = floor(Total * value))

school_totals <- upd_school %>% 
  group_by(LEAID, LEA_NAME, ST, SCHOOL_YEAR) %>% 
  summarize(total = sum(n)*(sum(n)-1))

upd_school_dvs <- upd_school %>% 
  group_by(LEAID, LEA_NAME, ST, SCHOOL_YEAR) %>% 
  mutate(n_minus = n-1) %>% 
  summarize(numerator = sum(n*n_minus)) %>% 
  left_join(school_totals) %>% 
  mutate(diversity = 1 - numerator/total) %>% 
  select(GEOID = LEAID, NAME = LEA_NAME, ST, school_diversity = diversity) 

# Environment Cleanup---
remove(list = ls()[str_which(ls(), "upd_school_dvs|acs_diversity", negate = TRUE)])

upd_school_dvs %>% 
  ggplot(aes(x = school_diversity)) +
  geom_histogram()
