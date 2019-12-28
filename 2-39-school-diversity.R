library(tidyverse)
library(tidycensus)


# get data ----------------------------------------------------------------


data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                         "master/data/2019/2019-09-24/school_diversity.csv")

# before running this, make a folder named data in your working directory
data_path <- "data/2-39-school-diversity.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
# YOU MUST MAKE a new FOLDER NAMED DATA
if(!file.exists(data_path)) {
  schools <- 
    data_url %>% 
    read_csv() %>% 
    janitor::clean_names()
  
  save(schools, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

schools %>% 
  map(~is.na(.) %>% sum())

schools %>% 
  ggplot(aes(x ))
