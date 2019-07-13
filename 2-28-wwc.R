library(tidyverse)

# -------------------------------------------------------------------------


data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")

data_path <- "data/2-28-wwc.Rdata"


if(!file.exists(data_path)) {
  wwc <- 
    data_url %>% 
    read_csv() %>% 
    distinct()
  
  save(wwc, file = data_path)
} else {
  load(data_path)
}



# explore -----------------------------------------------------------------

wwc %>% 
  filter(win_status == "Won") %>% 
  group_by(team) %>% 
  count() %>% 
  arrange(desc(n))

# viz ---------------------------------------------------------------------

wwc %>% 
  filter(year == 2019) %>% 
  group_by(team) %>% 
  summarise(n = n(),
            tot_goals = sum(score)) %>% 
  ggplot(aes(x = n,
             y = tot_goals)) +
  geom_point()

top_teams <- 
  c("USA", "GER", "NOR", "SWE", "BRA", "CHN", "ENG", "JPN", "FRA", "AUS")

wwc %>% 
  mutate(year_game = paste(year, yearly_game_id)) %>%
  filter(win_status == "Won",
         team %in% top_teams) %>% 
  ggplot(aes(x = year_game,
             y = team,
             shape = win_status)) +
  geom_point()


wwc %>% 
  group_by(year, team) %>% 
  arrange(year, yearly_game_id) %>% 
  mutate(match_n = 1:n()) %>% # filter(team == "USA") %>% View()
  filter(team %in% top_teams,
         round != "Third Place Playoff") %>% 
  ggplot(aes(x = match_n,
             y = team,
             shape = win_status)) +
  geom_point() +
  facet_grid(. ~ year)
