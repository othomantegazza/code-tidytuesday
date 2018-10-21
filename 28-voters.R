library(tidyverse)
library(magrittr)
library(rvest)

# Main opponents each year ----------------------------------------------

opponents_url <- paste0("https://en.wikipedia.org/wiki/", 
                        "List_of_United_States_presidential_candidates")

pull_lastname <- function(i) {
  i %>%
    str_split(pattern = "\\(", 
              n = 2,
              simplify = TRUE) %>%
    .[, 1] %>%
    str_split(pattern = " ",
              simplify = TRUE) %>%
    as.character() %>%
    tail(2) %>%
    .[1]
}

opponents <- 
  opponents_url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table() %>%
  select(-Other) %>% 
  mutate(Democratic = Democratic %>% map_chr(pull_lastname),
         Republican = Republican %>% map_chr(pull_lastname)) %>%
  transmute(year = Year,
            opponents = paste(Democratic, Republican, sep = " vs."))

save(opponents, file = "data/28-opponents.Rdata")

# Try read US census from pdf online ----------------------------------------

library(tabulizer)


census_url <- "https://www.census.gov/prod/2011pubs/12statab/election.pdf"

census_path <- "data/28-voter-census.Rdata"

if(!file.exists(census_path)) {
  census_table <- 
    census_url %>%
    extract_tables(pages = 2)
  
  save(census_table, file = census_path)
  
} else {
  load(census_path)
}


# Tidy census  ------------------------------------------------------------

census_tidy <- 
  # Its a matrix within a list
  census_table[[1]] %>%
  as_tibble() %>%
  # colnames are spread over first columns
  slice(8:n()) %>%
  # data from 1972 to 2010 
  select(V7, V8, V9, V10, V12) %>%
  # The year is in V7 and has some dots
  # at the end
  rename(year = "V7") %>%
  mutate(year = str_sub(year,
                        start = 1,
                        end = 4) %>% as.numeric()) %>%
  # The population in voting age is in V8
  # in the last lines the first 2 or 3 is actually a note
  rename(voting_pop = "V8") %>% 
  mutate(voting_pop = str_replace(voting_pop, "2 ", "") %>% 
           str_replace("3 ", "") %>%
           str_replace(",", "") %>%
           as.numeric()*1000) %>% 
  # The ones that voted for the President is in V9
  rename(presidential = "V9") %>%
  mutate(presidential = str_replace(presidential, ",", "") %>%
           na_if("(X)")%>%
           as.numeric()*1000) %>%  
  # V10 contains both 
  # the percentage that voted for the President
  # and the ones that voted for the representative
  mutate(president_perc= str_split_fixed(V10, " ", n = 2)[,1] %>%
           na_if("(X)") %>%
           as.numeric(),
         representative = str_split_fixed(V10, " ", n = 2)[,2] %>%
           str_replace(",", "") %>%
           as.numeric()*1000) %>%
  select(-V10) %>%
  # percentage that voted at representative is in V12
  rename(representative_perc = "V12") %>%
  mutate(representative_perc = as.numeric(representative_perc)) %>%
  # better order for columns
  select(year, 
         voting_pop,
         presidential,
         president_perc,
         representative,
         representative_perc)

# what is a reasonable way to gather this dataset?
census_to_plot <- 
  census_tidy %>% 
  select(-contains("perc")) %>%
  gather(key = "measure", value = "voters",
         presidential, representative, voting_pop) %>% 
  arrange(year)  %>% 
  filter(complete.cases(.)) 

tot_pop <- 
  census_to_plot %>% 
  filter(measure == "voting_pop") %>%
  select(year, 
         voting_pop = voters) %>%
  distinct()


census_to_plot <- 
  census_to_plot %>%
  left_join(tot_pop) %>%
  mutate(perc = voters/voting_pop) %>%
  select(-voting_pop) %>%
  left_join(opponents) 

# Plot --------------------------------------------------------------------

p <- census_to_plot %>%
  ggplot(aes(x = year %>% as.character() %>% as_factor() %>% fct_rev(),
             y = voters)) +
  geom_line(aes(group = year),
            lwd = 0.2) +
  geom_linerange(data = . %>%
                   filter(measure == "representative"),
                 aes(ymax = voters),
                 ymin = 0,
                 colour = "grey40",
                 lty = 2) +
  geom_point(aes(colour = measure),
             size = 2.5) +
  geom_text(data = . %>%
              filter(measure == "representative") %>%
              mutate(label = paste0(perc %>% round(3)*100, "%")),
            aes(label = label),
            size = 2.5,
            colour = "grey40",
            nudge_y = -1.2e+07,
            nudge_x = .35) +
  geom_text(data = . %>%
              filter(measure == "presidential") %>%
              mutate(label = paste0(perc %>% round(3)*100, "%")),
            aes(label = label),
            size = 2.5,
            colour = "grey40",
            nudge_y = +1.2e+07,
            nudge_x = .35) +
  geom_text(data = . %>%
              filter(measure == "voting_pop"),
            aes(label = opponents),
            size = 2.5,
            colour = "grey40",
            nudge_y = 5e+06,
            hjust = 0, 
            vjust = 0) +
  ylim(0, 3e+08) +
  coord_flip() +
  scale_color_manual(values = scico::scico(10, palette = "lajolla")[c(6, 8, 3)],
                     breaks = c("presidential",
                                "representative",
                                "voting_pop"),
                     labels = c("Voters,\nPresidential",
                                "Voters,\nRepresentative",
                                "Population\nof Voting Age")) +
  theme_minimal() +
  theme(legend.position = "top", legend.justification = 0) +
  guides(colour = guide_legend(title = NULL,
                               label.position = "right",
                               nrow=1)) +
  labs(title = "In US, How Many People of Age Do Vote?",
       subtitle = "From 1972 to 2010, split in Presidential and Representative elections",
       x = "",
       y = "voters [n]",
       caption = "Data: US Census - www.census.gov | Plot: @othomn")

png(filename = "plots/28-voters.png",
    height = 2000, width = 1700,
    res = 300)
p %>% print()
dev.off()
