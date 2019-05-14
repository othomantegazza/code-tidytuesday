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


# plot -----------------------------------------------------------------

p <- 
  nobel %>% 
  mutate(age =  prize_year - year(birth_date)) %>% 
  ggplot(aes(x = category,
             y = age,
             colour = gender,
             alpha = gender)) +
  ggbeeswarm::geom_beeswarm() +
  coord_flip() +
  scale_color_manual(values = c("#BB1288", "#5867A6")) +
  scale_alpha_manual(values = c(1, .4)) +
  theme_minimal() +
  labs(title = "Way Beyond Gender Imbalance",
       subtitle = "Nobel prize laureates until 2016",
       colour = "Gender",
       alpha = "Gender",
       x = "Category",
       y = "Age",
       caption = "Source: Kaggle | Plot by @othomn")

# I was wondering if with a beeswarm plot you can show also every nobel price as a point. The picture 

# some checks
nobel %>% filter(gender == "Female") %>% select(category, full_name) #%>% View()
nobel %>%
  mutate(age =  prize_year - year(birth_date)) %>% 
  filter(age < 25) %>% select(category, full_name) #%>% View()


# save --------------------------------------------------------------------

png(filename = "plots/2-20-nobel-prize.png",
    res = 300,
    height = 1800,
    width = 2400)
p %>% print()
dev.off()
