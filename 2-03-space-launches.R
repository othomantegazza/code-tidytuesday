library(tidyverse)

# Get Data ----------------------------------------------------------------


dat_path <- "data/2-03-space-launces.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-15/launches.csv")


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(dat_url)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}

# also agencies data

agencies_path <- "data/2-03-agencies.Rdata"
agencies_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "/2019/2019-01-15/agencies.csv")


if(!file.exists(agencies_path)) {
  dat_agencies <- 
    read_csv(agencies_url)
  
  save(dat_agencies, file = agencies_path)
  
} else {
  load(agencies_path)
}


# Explore -----------------------------------------------------------------

dat %>% View()

dat %>% 
  filter(state_code == "US") %>% 
  ggplot(aes(x = launch_year,
             fill = agency_type)) +
  geom_bar()
