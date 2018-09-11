library(tidyverse)
library(statebins)
library(socviz)

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


# plot --------------------------------------------------------------------

# replicate https://socviz.co/maps.html#map-u.s.-state-level-data

# get the census column from socviz
# to facet macro-regions

tst <- dat %>%
  left_join(socviz::election %>%
              select(state, census)) %>%
  mutate(ratio_owner = percent_dog_owners/percent_cat_owners,
         main_household = case_when(ratio_owner > 1 ~ "Dog",
                                    ratio_owner < 1 ~ "Cat",
                                    TRUE ~ "Both"))

# tst <- dat %>%
#   left_join(socviz::election %>%
#               select(state, census)) %>%
#   mutate(ratio_owner = avg_dogs_per_household/avg_cats_per_household,
#          main_household = case_when(ratio_owner > 1 ~ "Dog",
#                                     ratio_owner < 1 ~ "Cat",
#                                     TRUE ~ "Both"))
#   

tst %>%
  ggplot(aes(x = reorder(state, ratio_owner),
             y = ratio_owner,
             colour = main_household)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1) +
  facet_grid(census ~ .,
             scales = "free_y",
             space = "free") +
  coord_flip() +
  theme_minimal()






