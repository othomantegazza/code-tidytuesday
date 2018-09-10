library(tidyverse)


# Get data ----------------------------------------------------------------

dat_path <- "data/24-cats-and-dogs.Rdata"


if(!file.exists(dat_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2018-09-11/cats_vs_dogs.csv")) %>%
    select(-X1)
  save(dat, file = dat_path)
} else {
  load(dat_path)
}

