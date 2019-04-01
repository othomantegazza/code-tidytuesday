# show small effects adding arrows to every ridges.

# ON SIMULATED DATA !!!!!!


# load packages -----------------------------------------------------------

library(tidyverse)
library(ggridges)


# reproduce ---------------------------------------------------------------

set.seed(1)

# simulate random data ----------------------------------------------------

dat <- tibble(`reference group` = rnorm(400, 10),
              group1 = map(rnorm(4, 1, .05), ~rnorm(100, mean = 11*.)) %>% reduce(c),
              group2 = map(rnorm(4, 1, .075), ~rnorm(100, mean = 12.1*.)) %>% reduce(c),
              group3 = map(rnorm(4, 1, .066), ~rnorm(100, mean = 9*.)) %>% reduce(c)) %>% 
  gather()


# plot ridges function ----------------------------------------------------

plot_ridge_perc <- function(df = dat,
                            key_ref = "reference group") 
{
  some_stats <- 
    dat %>% 
    group_by(.data$key) %>% 
    summarise(med = median(.data$value))
  
  # median WT
  med_ref <- some_stats %>%
    filter(key == key_ref) %>%
    pull(med)
  
  # also percent change
  some_stats <- 
    some_stats %>% 
    mutate(percent = (.data$med - med_ref)*100/med_ref,
           y = 1:n() - .15) 
  
  # some parameters
  color_annos <- "grey20"
  color_points <- "#3752C3"
  
  p <- dat %>% 
    ggplot(aes(x = value,
               y = key)) +
    geom_density_ridges(rel_min_height = .02,
                        scale = .7,
                        quantile_lines = TRUE,
                        colour = color_points,
                        fill = "grey70",
                        # fill = "#263A89",
                        alpha = .5,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.05,
                                                          height = 0),
                        point_shape = '|',
                        point_size = 2, 
                        point_colour = color_points, #"grey20",
                        point_alpha = .3) +
    geom_vline(xintercept = med_ref,
               linetype = 2,
               colour = color_annos) +
    geom_segment(data = some_stats %>% 
                   filter(key != key_ref),
                 aes(xend = med,
                     y = y,
                     yend = ..y..),
                 x = med_ref,
                 arrow = arrow(length = unit(1, "mm"),
                               type = "closed"),
                 colour = color_annos) +
    geom_text(data = some_stats %>% 
                filter(key != key_ref),
              aes(x = med,
                  label = case_when(percent >= 0 ~ percent %>%
                                      round(1) %>%
                                      {paste0("+", ., "%")},
                                    TRUE ~ percent %>%
                                      round(1) %>%
                                      paste("%")),
                  y = y,
                  hjust = case_when(percent >= 0 ~ 0, TRUE ~ 1)),
              # vjust = 0,
              # nudge_y = .1,
              size = 3,
              colour = color_annos) +
    scale_fill_viridis_d(option = "D",
                         guide = FALSE) +
    # scico::scale_fill_scico_d(begin = .2, end = .8,
    #                           palette = "lajolla",
    #                           guide = FALSE) +
    labs(y = "",
         caption = "Plot done in ggplot2 by @othomn",
         title = "Ridges and Percent Changes") +
    theme_ridges() +
    theme(axis.text = element_text(size = 10,
                                   colour = color_annos),
          text =  element_text(size = 11,
                               colour = color_annos),
          plot.title = element_text(size = 14,
                                    colour = color_annos,
                                    face = "plain", margin = margin(0,0,20,0)))
  
  p
}


# plot --------------------------------------------------------------------

p <- plot_ridge_perc()
p

# save plot ---------------------------------------------------------------

png(filename = "plots/extra-ridges-percent.png",
    width = 1700, height = 1300,
    res = 300)
p
dev.off()

