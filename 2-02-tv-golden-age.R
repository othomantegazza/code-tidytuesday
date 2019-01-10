library(tidyverse)
library(lubridate)
library(rlang)
library(magrittr)
library(broom)


# Set parameters ----------------------------------------------------------

light_orange <- "orange"
dark_orange <- "#E97E00"
title_blue <- "#5F3BBA"
highlight_blue <- "#46AEF9"

# Set theme and functions -------------------------------------------------

theme_set(theme_gray() +
            theme(legend.position = "right",
                  text = element_text(family = "Arial Narrow",
                                      colour = "grey40",
                                      size = 14),
                  plot.title = element_text(colour = title_blue, # or  "#582F69"
                                            face = "bold",
                                            size = 27,
                                            family = "Arial Narrow"),
                  plot.subtitle = element_text(colour = "grey40",
                                               face = "bold",
                                               size = 12),
                  aspect.ratio = .67))


save_plot <- function(p, file) 
{
  on.exit(
    dev.off()
  )
  png(filename = file,
      height = 1500, width = 2200,
      res = 300)
  p %>% print()
}

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

dat_drama <- 
  dat %>% 
  filter(genres %>% str_detect("Drama"))

p <- 
  dat_drama %>% 
  ggplot(aes(x = date, y = av_rating, colour = share)) +
  geom_point(alpha = .8, size = 2) +
                        breaks = c(.01, .1, 1, 10, 50),
  scale_color_viridis_c(trans = "log10",
                        # limits = c(NA, 100)
                        guide = guide_colourbar(barheight = 12)) +
  labs(x = "Original Airing Date",
       y = "Average Rating",
       colour = str_wrap("Share of yearly votes", width = 12),
       caption = "Data: IMDB | Plot by @othomn")


save_plot(p = p +
            labs(title = "Series with high rating get more votes"),
          file = "plots/2-02-1-tv.png")

# How to describe it?
# 1. Is it normally distributed?
# 2. There is this issue with low share ratings :
#   - https://www.evanmiller.org/how-not-to-sort-by-average-rating.html
#   - https://redditblog.com/2009/10/15/reddits-new-comment-sorting-system/
# 3. For best series share is higher, (they get more attention?)
#   - It requires a slump to get high share and bad rating

#  Most of the high share ratings are above the regression line -----------

fit <- 
  dat_drama %>% 
  {lm(av_rating ~ poly(date, 3), data = .)}

glance(fit)
tidy(fit)
fit %>% predict(newdata = dat_drama)

p_lm <- 
  p +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              colour = light_orange)

save_plot(p = p_lm +
            annotate(geom = "label",
                     x = as_date("2013-10-01"), y = 8.5,
                     label = "y ~ poly(x, 3)",
                     fill = dark_orange,
                     colour = "white",
                     size = 4) +
            labs(title = "Do ratings form trend over time? Not sure"),
          file = "plots/2-02-2-tv.png")

# Is Star Trek DS9 the only one below? ------------------------------------

# base plot for highlightings

p_labs <- 
  p_lm +
  ggrepel::geom_label_repel(data = . %>% 
                              mutate(pred_lm = fit %>% predict(newdata = dat_drama),
                                     year = year(date)) %>% 
                              group_by(year) %>% 
                              top_n(n = 1, wt = share) %>% 
                              # filter(av_rating < 8),
                              filter(av_rating < pred_lm),
                           aes(label = paste0(title,
                                              ", season ",
                                             seasonNumber) %>%
                                 str_wrap(width = 20)),
                           fontface = "bold",
                           fill = dark_orange,
                           angle = 90,
                           colour = "white",
                           segment.colour = dark_orange,
                           box.padding = 0,
                           ylim = c(NA, 6)) + 
  geom_point(data = . %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               top_n(n = 1, wt = share),
             shape = 21,
             colour = title_blue,
             fill = light_orange,
             stroke = 1.6,
             size = 3) 

  
# p_labs

save_plot(p = p_labs +
            labs(title = str_wrap("Two Series have best yearly share of
                 votes but low ratings")) +
            theme(plot.title = element_text(size = 21)), 
          file = "plots/2-02-3-tv.png")


