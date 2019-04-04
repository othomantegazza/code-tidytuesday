library(tidyverse)

pet_names <- read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-03-26/seattle_pets.csv?raw=true",
                col_types = cols(license_issue_date = col_datetime(format = "%B %d %Y")))


dat2 %>% 
  ggplot(aes(x = license_issue_date)) +
  geom_density(fill = "blue")
