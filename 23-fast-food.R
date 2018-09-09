library(tidyverse)
library(ggrepel)
library(broom)

# Get data ----------------------------------------------------------------

dat_path <- "data/23-fast-food.Rdata"


if(!file.exists(dat_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/master/",
                         "data/2018-09-04/fastfood_calories.csv")) %>%
    select(-X1)
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# Check words in item's name ----------------------------------------------


get_kword_cals <- function(kword)
{
  print(kword)
  dat %>%
    filter(str_detect(item,
                      coll(kword))) %>%
    mutate(keyword = kword) #%>%
    # select(keyword, calories)
}

dat_kword <- str_split(dat$item, " ") %>%
  unlist() %>%
  unique() %>%
  # str_remove("\(") %>%
  map(get_kword_cals) %>%
  purrr::reduce(bind_rows)
  

dat_kword <- dat_kword %>%
  group_by(keyword) %>%
  add_tally() %>%
  filter(n > 10)


png(filename = "plots/23-fast-food.png",
    height = 2500, width = 1400,
    res = 300)
dat_kword %>% 
  ggplot(aes(x = reorder(keyword, calories),
             y = calories)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(alpha = .1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5,
                                   hjust = 1)) +
  coord_flip() +
  labs(title = "Which Word Brings Most Calories?",
       subtitle = "In fast-food meal names",
       y = "Calories",
       x = "Word",
       caption = "Source: fastfoodnutrition.org, plot by @othomn")
dev.off()
  
