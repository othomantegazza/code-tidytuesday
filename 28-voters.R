library(tidyverse)
library(lubridate)
library(rvest)

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


# Parse wikipedia ---------------------------------------------------------

elect_80 <- read_html("https://en.wikipedia.org/wiki/United_States_presidential_election,_1980")


# The table with Elector number is after the h2 "Statistics"

# Get all h2 and all tables
elect_nodes <- 
  elect_80 %>%
  html_nodes("h2,table")

hit <- which(elect_nodes %>% html_text() == "Statistics[edit]")

# 
voters <- 
  elect_nodes[[hit + 1]] %>%
  html_table(fill = TRUE)

# Get also the results by state

h3_tables <- 
  elect_80 %>%
  html_nodes("h3,table")

hit_state <- 
  h3_tables %>%
  html_text() %>%
  str_detect("Results by state") %>%
  which()

by_state <- 
  h3_tables[[hit_state + 2]] %>%
  html_table()


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
