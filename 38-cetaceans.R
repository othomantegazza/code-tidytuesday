library(tidyverse)
library(lubridate)
library(rlang)
library(gtable)
library(gridExtra)
library(grid)
library(scico)

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

# prepare data for plots
dat_acq <- 
  dat %>% 
  filter(acquisition %in% c("Born", "Capture", "Rescue")) %>% 
  mutate(acquisition = factor(
    acquisition, levels = c("Rescue", "Born", "Capture")
  )) 


# set colors
# sc_pal <- scico::scico(10, palette = "lajolla")[c(8, 6, 3)]
sc_pal <- scico::scico(10, palette = "devon")[c(1, 4, 6)]

p_dens <- 
  dat_acq %>% 
  ggplot(aes(x = originDate,
             y = stat(count),
             fill = acquisition)) +
  # geom_histogram() +
  geom_density(alpha = .5) +
  scale_fill_manual(
    values = sc_pal,
    guide = guide_legend(title.vjust = .2,
                         label.position = "top",
                         keyheight = unit(4, units = "mm"),
                         keywidth=unit(14, units = "mm"), 
                         nrow = 1,
                         reverse = TRUE)) +
  # guides(fill = FALSE) +
  theme_bw() +
  labs(x = NULL, 
       y = "Density",
       title = "New Cetaceans in Captivity in the US",
       subtitle = "Since the '90s, no new cetacean has been captured",
       fill = "Mean of Acquisition") +
  theme(plot.title = element_text(colour = "grey20",
                                  face = "bold",
                                  size = 18, family = "Arial Narrow"),
        plot.subtitle = element_text(colour = "grey40",
                                     face = "bold",
                                     size = 12),
        aspect.ratio = .35,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 0, l = 3, unit = "mm"),
        legend.position = "top")

# p_dens

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
  scale_color_manual(values = sc_pal) +
  guides(colour = FALSE) +
  theme_bw() +
  theme(aspect.ratio = .12,
        plot.margin = margin(t = 0, r = 10, b = 10, l = 3, unit = "mm")) +
  labs(x = "Day of Acquisiton",
       y = "",
       caption = "Sources: FOIA, Ceta-Base; collected by Amber Thomas | Plot by @othomn")

# p_point


# Put them together -------------------------------------------------------

png(filename = "plots/38-cetaceans.png",
    height = 1600, width = 2200,
    res = 300)
grid.newpage()
gtable_rbind(p_dens %>% ggplotGrob(),
             p_point %>% ggplotGrob(),
             size = "max") %>% 
  grid.draw()
dev.off() 

