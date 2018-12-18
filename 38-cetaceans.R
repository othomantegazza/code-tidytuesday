library(tidyverse)
library(glue)
library(lubridate)
library(rlang)

# Get Data ----------------------------------------------------------------


dat_path <- "data/38-cetaceans.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2018-12-18/allCetaceanData.csv")


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(dat_url
             # col_types = cols(
             #   birthYear = col_double()
             # )
             )
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


# Wrangle -----------------------------------------------------------------

# dat <- 
#   dat %>% 
#   mutate(birthYear = as.numeric(birthYear))


# Explore -----------------------------------------------------------------

dat %>% 
  filter(status == "Died") %>% 
  ggplot(aes(x = birthYear %>% as.numeric(), 
             y = statusDate %>% year())) +
  geom_point() +
  facet_grid(. ~ acquisition)

dat_acq <- 
  dat %>% 
  filter(acquisition %in% c("Born", "Capture", "Rescue")) %>% 
  mutate(acquisition = factor(
    acquisition, levels = c("Rescue", "Born", "Capture")
  )) 

p_dens <- 
  dat %>% 
  filter(acquisition %in% c("Born", "Capture", "Rescue")) %>% 
  ggplot(aes(x = originDate,
             y = stat(count),
             fill = acquisition)) +
  # geom_histogram() +
  geom_density(alpha = .5) +
  # geom_rug() +
  # facet_grid(acquisition ~ .) +
  guides(fill = FALSE) +
  theme_bw()

p_dens

half_width <- .33

p_point <- 
 dat_acq %>% 
  ggplot(aes(x = originDate,
             y = acquisition %>% as.numeric(),
             colour = acquisition)) +
  # geom_jitter(width = 0, height = .1,
  #             alpha = .3) +
  geom_linerange(aes(ymin = as.numeric(acquisition) - half_width,
                     ymax = as.numeric(acquisition) + half_width),
                 lwd = .2) +
  scale_y_continuous(breaks = 1:3,
                     labels = levels(dat_acq$acquisition)) +
  guides(colour = FALSE) +
  theme_bw()

p_point


# Put them together -------------------------------------------------------

library(gtable)
library(grid)
grid.newpage()
gtable_rbind(p_dens %>% ggplotGrob(),
             p_point %>% ggplotGrob()) %>% 
  grid.draw()
