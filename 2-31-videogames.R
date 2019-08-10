library(tidyverse)


# get data ----------------------------------------------------------------

"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv"

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience",
                   "/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

data_path <- "data/2-31-videogames.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  videogames <- 
    data_url %>% 
    read_csv() 
  
  save(videogames, file = data_path)
} else {
  load(data_path)
}
