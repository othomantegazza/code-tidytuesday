library(tidyverse)


# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-08-06/bob-ross.csv")

data_path <- "data/2-32-paintings.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  paintings <- 
    data_url %>% 
    read_csv() %>% 
    janitor::clean_names()
  
  save(paintings, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

paintings %>% View()

paintings %>% 
  select(-title) %>%
  column_to_rownames("episode") %>%
  superheat::superheat(row.dendrogram = TRUE,
                       bottom.label.text.angle = 90,
                       bottom.label.text.size = 3)
  
