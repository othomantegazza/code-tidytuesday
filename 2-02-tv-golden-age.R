library(tidyverse)
library(lubridate)
library(rlang)
library(gtable)
library(gridExtra)
library(grid)
library(scico)
library(magrittr)


# Set theme and functions -------------------------------------------------

theme_set(theme_bw() + 
            theme(legend.position = "right",
                  text = element_text(family = "Arial Narrow",
                                      colour = "grey40"),
                  plot.title = element_text(colour = "grey20",
                                            face = "bold",
                                            size = 18, family = "Arial Narrow"),
                  plot.subtitle = element_text(colour = "grey40",
                                               face = "bold",
                                               size = 12),
                  aspect.ratio = .70))

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


# Basic plot, then add layer ----------------------------------------------


p <- 
dat %>%
  filter(genres %>% str_detect("Drama")) %>% 
  ggplot(aes(x = date, y = av_rating, colour = share)) +
  geom_point(alpha = .8) +
  scale_color_viridis_c(trans = "log10",
                        breaks = c(.01, .1, 1, 10, 50)
                        # limits = c(NA, 100)
                        ) +
  labs(x = "Original Airing Date",
       y = "Average Rating",
       colour = str_wrap("Share of yearly votes", width = 12))

# How to describe it?
# 1. Is it normally distributed?
# 2. There is this issue with low share ratings :
#   - https://www.evanmiller.org/how-not-to-sort-by-average-rating.html
#   - https://redditblog.com/2009/10/15/reddits-new-comment-sorting-system/
# 3. For best series share is higher, (they get more attention?)
#   - It requires a slump to get high share and bad rating

#  Most of the high share ratings are above the regression line -----------

p + labs(title = "Series with high rating get more votes")

p_lm <- 
  p +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              colour = "orange")

# Is Star Trek DS9 the only one below? ------------------------------------

# base plot for highlightings


p_lm +
  geom_point(data = . %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               top_n(n = 1, wt = share),
             colour = "orange", size = 3) +
  ggrepel::geom_text_repel(data = . %>% 
                              mutate(year = year(date)) %>% 
                              group_by(year) %>% 
                              top_n(n = 1, wt = share) %>% 
                              filter(av_rating < 8),
                           aes(label = title), 
                           angle = 90,
                           colour = "orange",
                           ylim = c(NA, 6))
  


tst <- 
  dat %>% 
  filter(genres %>% str_detect("Drama")) %>% 
  mutate(year = date %>% year() %>% as.factor())

tst %>% 
  ggplot(aes(x = year, y = av_rating)) +
  # geom_boxplot() +
  stat_summary(fun.y = "median", fun.ymax = "median", fun.ymin = "median",
               na.rm = TRUE,
               colour = "orange",
               geom = "crossbar", width = 0.5) +
  

tst %>% 
  group_by(year) %>% 
  top_n(n = 1, wt = share) %>% 
  arrange(year) %T>% 
  View()
  

# Test a polynomial regr --------------------------------------------------

dat %>% 

dat %>%
  filter(genres %>% str_detect("Drama")) %>% 

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


# ratio vs av_rating ------------------------------------------------------

dat %>% 
  ggplot(aes(x = av_rating, y = share)) +
  geom_hex()


genre_df %>% 
  filter(genre_id == "Drama") %>%
  slice(1668)
