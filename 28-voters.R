library(tidyverse)
library(lubridate)
library(rvest)
library(googlesheets)

# Get data ----------------------------------------------------------------

dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/",
                  "data/2018-10-09/voter_turnout.csv")

dat_path <- "data/28-voter-turnout.Rdata"

if(!file.exists(dat_path)) {
  dat <- read_csv(dat_url)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}

# Filter full census ------------------------------------------------------

dat <- 
  # dat %>% filter(state == "United States") 
  dat %>%
  filter(! state %>% str_detect("United States")) %>%
  filter(complete.cases(.)) %>%
  group_by(year) %>%
  summarise(votes = sum(votes, na.rm = TRUE),
            eligible_voters = sum(eligible_voters, na.rm = TRUE)) %>%
  gather(key = "variable", value = "voters",
         votes, eligible_voters) %>%
  mutate(variable = case_when(variable == "eligible_voters" ~ "eligible_voters",
                              year %in% seq(1982, 2014, 4) ~ "votes_midterm",
                              year %in% seq(1980, 2012, 4) ~ "votes_presidential"))


# Plot full census --------------------------------------------------------

dat %>%
  ggplot(aes(x = year %>% as.character() %>% as_factor() %>% fct_rev(),
             y = voters)) +
  geom_line(aes(group = year),
            lwd = 0.2) +
  geom_linerange(data = . %>%
                   filter(variable %in% c("votes_midterm", 
                                          "votes_presidential")),
                 aes(ymax = voters),
                 ymin = 0,
                 colour = "grey",
                 lty = 2) +
  geom_point(aes(colour = variable),
             size = 2.5) +
  ylim(0, NA) +
  # scale_x_reverse() +
  coord_flip() +
  scale_color_manual(values = scico::scico(10, palette = "lajolla")[c(8, 6, 3)]) +
  # scico::scale_colour_scico(begin = .2, end = .8, palette = "berlin") +
  # scale_colour_viridis_d(begin = .2, end = .9,
  #                        option = "E") +
  # ggthemes::scale_color_few() +
  theme_minimal()

# Many NA, don't know if it is reliable.

# Get presidentual elections from us election data ------------------------

# years with presidential election since the 80s
pres_years <- seq(1980, 2012, 4)

pres_urls <- paste0("https://uselectionatlas.org",
                "/RESULTS/data.php?year=",
                pres_years,
                "&datatype=national&def=1&f=0&off=0&elect=0")

parse_pres <- function(url) 
{
  read_html(url) %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE)
}

by_state <- 
  pres_urls %>%
  map(parse_pres)


# Voters turnout percentages ----------------------------------------------

# presidentials and midterm

# from http://www.electproject.org/national-1789-present
 
perc_url <- paste0("https://docs.google.com/spreadsheets/d/",
                   "1bH38j6_e8yA9xq8OMlyLOL6h_iTS7ABQMKNxzFgKBDo/",
                   "edit?usp=sharing")

# the package googlesheets didn't work in this case
# luckily rvest parses it as an html table
perc_votes <- 
  perc_url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(header = FALSE) %>%
  as_tibble() %>%
  slice(3:n()) %>%
  mutate_all(as.numeric)
  
# the columns 2 and 3 encode for presidential years
pres_percent <- 
  perc_votes %>%
  select(year = "X2",
         percent = "X3")

# 4 and 5 for midterms
midterm_percent <- 
  perc_votes %>% 
  select(year = "X4",
         percent = "X5")

# Voters turnout presidentials --------------------------------------------

pres_turnout <- 
  paste0("https://en.wikipedia.org/wiki/",
         "Voter_turnout_in_the_United_States_presidential_elections") %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() %>%
  as_tibble() %>%
  rename(year = "Election",
         voting_pop = "Voting Age Population (VAP)[clarification needed][8]",
         votes = "Turnout[clarification needed][8]") %>%
  select(year, voting_pop, votes) %>%
  filter(year %in% 1980:2012) %>%
  mutate_all(~str_replace_all(.,",", "")) %>%
  mutate_all(as.numeric) %>%
  gather(key = "variable",
         value = "value",
         voting_pop, votes)
  
pres_turnout %>%
  ggplot(aes(x = year,
             y = value)) +
  geom_line(aes(group = year),
            lwd = 0.2) +
  geom_linerange(data = pres_turnout %>%
                   filter(variable == "votes"),
                 aes(ymax = value),
                 ymin = 0,
                 colour = "grey",
                 lty = 2) +
  geom_point(aes(colour = variable),
             size = 2.5) +
  ylim(0, NA) +
  # scale_x_reverse() +
  coord_flip() +
  scale_color_manual(values = scico::scico(10, palette = "lajolla")[c(8, 6, 3)]) +
  # scico::scale_colour_scico(begin = .2, end = .8, palette = "berlin") +
  # scale_colour_viridis_d(begin = .2, end = .9,
  #                        option = "E") +
  # ggthemes::scale_color_few() +
  theme_minimal()


