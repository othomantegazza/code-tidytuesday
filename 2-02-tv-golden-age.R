library(tidyverse)
library(lubridate)
library(rlang)
library(gtable)
library(gridExtra)
library(grid)
library(scico)

# Get Data ----------------------------------------------------------------


dat_path <- "data/2-02-tv.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-08/IMDb_Economist_tv_ratings.csv")


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(dat_url)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}

# Explore genres rating ---------------------------------------------------

genre_types <- 
  dat %>% 
  pull(genres) %>% 
  map(~str_split(., pattern = ",", 
                 simplify = TRUE)) %>% 
  flatten_chr() %>% 
  unique() %>% 
  set_names()

genre_df <- 
  genre_types %>% 
  map(~filter(dat, str_detect(string = genres, pattern = .)))

genre_df <- 
  names(genre_df) %>% 
  map(~mutate(genre_df[[.]], genre_id = .)) %>% 
  reduce(bind_rows) 

genre_df %>% 
  ggplot(aes(date, av_rating)) +
  geom_point() +
  facet_wrap(facets = "genre_id") 


genre_df %>% 
  ggplot(aes(date, share)) +
  geom_point() +
  geom_point(data = . %>% filter(str_detect(title, "Trek")),
             colour = "red") +
  facet_wrap(facets = "genre_id") 


genre_df %>% 
  ggplot(aes(date, share)) +
  geom_point() +
  geom_point(data = . %>% filter(str_detect(title, "Trek")),
             colour = "red") +
  geom_label(data = . %>% filter(share > 20),
             aes(label = title))

genre_df %>% 
  ggplot(aes(date, av_rating)) +
  geom_hex() +
  geom_point(data = . %>% filter(genre_id == "Sci-Fi"),
             colour = "red") 

genre_df
# mutate(genres = map(
#   genres,
#   ~str_split(., pattern = ",", 
#              simplify = TRUE) %>% as.character())) %>% 

genre_df %>% filter(genre_id == "Documentary")
genre_df %>% filter(genre_id == "Reality-TV")

# Explore share ----------------------------------------------------------

dat %>%
  mutate(year = year(date)) %>%
  ggplot(aes(x = year, y = log(share))) +
  geom_point()

dat %>%
  ggplot(aes(x = date, y = log(share))) +
  geom_hex() +
  # geom_point() +
  stat_smooth(method = "lm") +
  scale_fill_viridis_c()


# Check leverage ----------------------------------------------------------

genre_df %>% 
  filter(genre_id == "Drama") %>% 
  ggplot(aes(date, av_rating, size = share)) +
  geom_point(colour = "blue", alpha = .4) +
  geom_smooth(colour = "red") +
  stat_smooth(method = "lm", colour = "darkred")


w_lm <- 
  genre_df %>% 
  filter(genre_id == "Drama") %>% 
  mutate(share = case_when(share == 0 ~ share + .001,
                           TRUE ~ share)) %>% 
  {lm(av_rating ~ date, data = ., weights = .$share)}

w_lm %>% summary()

library(ggfortify)
w_lm %>% autoplot()

library(broom)

# plot(w_lm)

augment(w_lm)
glance(w_lm)
tidy(w_lm)

w_lm %>% 
  augment() %>% 
  ggplot(aes(.fitted, .resid, size = X.weights.)) +
  geom_point()
  
# Remove outlier 1668 - DS9 S1

w_lm_ds9 <- 
  genre_df %>% 
  filter(genre_id == "Drama") %>%
  slice(-1668) %>% 
  mutate(share = case_when(share == 0 ~ share + .001,
                           TRUE ~ share)) %>% 
                           {lm(av_rating ~ date, data = ., weights = .$share)}

w_lm_ds9 %>% autoplot()

w_lm %>% tidy()
