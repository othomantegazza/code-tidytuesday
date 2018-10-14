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

# years with presidential election since the 80s
pres_years <- seq(1980, 2012, 4)

pres_htmls <-
  pres_years %>%
  # all html pages from wikipedia
  {paste0("https://en.wikipedia.org/wiki/",
          "United_States_presidential_election,_",
          .)} %>%
  map(read_html) 

# parse the number of electors from wikipedia pages
# wiki_html <- 
#   paste0("https://en.wikipedia.org/wiki/",
#          "United_States_presidential_election,_",
#          1980) %>%
#   read_html()

parse_electors <- function(wiki_html) 
  {
  # The table with Elector number is after the h2 "Statistics"
  nodes <- 
    wiki_html %>%
    # get h2s h3s and tables
    html_nodes("h2,h3,table")
  
  # which node is ""Statistics[edit]"
  hit <- 
    nodes %>%
    html_text() %>%
    {which(. == "Statistics[edit]")}
  
  print(hit)
  
  # the table is the node right after
  voters_table <- 
    nodes[hit + 1] %>%
    html_table(fill = TRUE)
}

voters_stats <- 
  pres_htmls %>%
  map(parse_electors)
  


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



# Try from other websites -------------------------------------------------

# years with presidential election since the 80s
pres_years <- seq(1980, 2012, 4)

url_2 <- paste0("https://uselectionatlas.org",
                "/RESULTS/data.php?year=",
                pres_years,
                "&datatype=national&def=1&f=0&off=0&elect=0")

get_table2 <- function(url) 
{
  read_html(url) %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE)
}

by_state <- 
  url_2 %>%
  map(get_table2)


# Voters turnout percentages ----------------------------------------------

# presidentials and midterm

# from http://www.electproject.org/national-1789-present
 
"https://docs.google.com/spreadsheets/d/1bH38j6_e8yA9xq8OMlyLOL6h_iTS7ABQMKNxzFgKBDo/edit#gid=435419492"


# Voters turnout presidentials --------------------------------------------

"https://en.wikipedia.org/wiki/Voter_turnout_in_the_United_States_presidential_elections"

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
