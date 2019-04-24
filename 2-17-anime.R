library(tidyverse)

# get data ----------------------------------------------------------------

anime_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                "tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
anime_file <- "data/2-17-anime.Rdata"


if(!file.exists(anime_file)) {
  anime <- readr::read_csv(anime_url)
  
  save(anime, file = anime_file)
  
} else {
  load(anime_file)
}


# Explore -----------------------------------------------------------------


