library(tidyverse)


# Get data ----------------------------------------------------------------

dat_path <- "data/fifa.Rdata"
if(!file.exists(dat_path)) {
  dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week11_fifa_audience.csv",
                  col_types = cols_only(country = col_character(),
                                        confederation = col_character(),
                                        population_share = col_double(),
                                        tv_audience_share = col_double(),
                                        gdp_weighted_share = col_double()))
  save(dat, file = dat_path)
} else {
  load(dat_path)
}

