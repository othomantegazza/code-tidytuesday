library(tidyverse)
library(lubridate)


# Get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-05-14/nobel_winners.csv")

data_path <- "data/2-20-nobel-prize.Rdata"

if(!file.exists(data_path)) {
  nobel <- read_csv(data_url)
  
  save(nobel, file = data_path)
} else {
  load(data_path)
}


# Explore -----------------------------------------------------------------

nobel %>% 
  mutate(age =  prize_year - year(birth_date)) %>% 
  ggplot(aes(x = category,
             y = age,
             colour = gender)) +
  ggbeeswarm::geom_beeswarm(alpha = .8) +
  coord_flip() +
  theme_bw()

# I was wondering if with a beeswarm plot you can show also every nobel price as a point. The picture 