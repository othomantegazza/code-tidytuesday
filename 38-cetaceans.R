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
    read_csv(dat_url)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}

# Plot -----------------------------------------------------------------

# prepare data for plots
dat_acq <- 
  dat %>% 
  filter(acquisition %in% c("Born", "Capture", "Rescue")) %>% 
  mutate(acquisition = factor(
    acquisition, levels = c("Rescue", "Born", "Capture")
  )) 


# set colors
sc_pal <- scico::scico(10, palette = "devon")[c(1, 4, 6)]


# Put a density plot on top
p_dens <- 
  dat_acq %>% 
  ggplot(aes(x = originDate,
             y = stat(count),
             fill = acquisition)) +
  geom_density(alpha = .5) +
  scale_fill_manual(
    values = sc_pal,
    guide = guide_legend(title.vjust = .2,
                         label.position = "top",
                         keyheight = unit(4, units = "mm"),
                         keywidth=unit(14, units = "mm"), 
                         nrow = 1,
                         reverse = TRUE)) +
  theme_bw() +
  labs(x = NULL, 
       y = "Density",
       title = "New Cetaceans in Captivity in the US",
       subtitle = "Since the '90s, no new cetacean has been captured",
       fill = "Mean of Acquisition") +
  theme(text = element_text(family = "Arial Narrow",
                             colour = "grey40"),
        plot.title = element_text(colour = "grey20",
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


# and a rug at the bottom

half_width <- .33

p_point <- 
 dat_acq %>% 
  ggplot(aes(x = originDate,
             y = acquisition %>% as.numeric(),
             colour = acquisition)) +
  geom_linerange(aes(ymin = as.numeric(acquisition) - half_width,
                     ymax = as.numeric(acquisition) + half_width),
                 lwd = .2) +
  scale_y_continuous(breaks = 1:3,
                     labels = levels(dat_acq$acquisition)) +
  scale_color_manual(values = sc_pal) +
  guides(colour = FALSE) +
  theme_bw() +
  theme(aspect.ratio = .12,
        plot.margin = margin(t = 0, r = 10, b = 10, l = 3, unit = "mm"),
        text = element_text(family = "Arial Narrow",
                            colour = "grey40")) +
  labs(x = "Day of Acquisiton",
       y = "",
       caption = "Sources: FOIA, Ceta-Base; collected by Amber Thomas | Plot by @othomn")



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


# Try logistic regression -------------------------------------------------

# For acquisition: use two levels:
# - Capture: the original and less respectful method
# - Others: Born +  Rescue = more modern and respectful methods

dat_acq2 <- 
  dat_acq %>%
  arrange(originDate) %>% 
  # filter(acquisition %in% c("Born", "Capture")) %>% 
  # mutate(acquisition = forcats::fct_relevel(acquisition)) %>% pull(acquisition)
  mutate(acquisition = acquisition %>% 
           fct_collapse(Others = c("Born", "Rescue")) %>% 
           fct_inorder(f = .))#levels = c("Capture", "Others"))) #pull(acquisition)

# reset colour palette with two levels
# set colors
sc_pal2 <- sc_pal[c(2,3,1)]

# Put a density plot on top, again
p_dens <- 
  dat_acq2 %>% 
  ggplot(aes(x = originDate,
             y = stat(count),
             fill = acquisition)) +
  geom_density(alpha = .5) +
  scale_fill_manual(
    labels = c("Capture", "Others\n[Born or Rescue]"),
    values = sc_pal2,
    guide = guide_legend(title.vjust = .2, 
                         label.position = "top",
                         keyheight = unit(4, units = "mm"),
                         keywidth=unit(14, units = "mm"), 
                         nrow = 1)) +
  theme_bw() +
  labs(x = NULL, 
       y = "Density\nNew Captives per Day",
       title = "New Cetaceans in Captivity in the US",
       subtitle = str_wrap("With a classifier that estimates the probability that
                           a new cetacean was acquired by friendly means [Not Captured]
                           as a function of the Day of Acquisition"),
       fill = "Mean of Acquisition") +
  theme(text = element_text(family = "Arial Narrow",
                            colour = "grey40"),
        plot.title = element_text(colour = "grey20",
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

p_dens

# fit logistic regression

fit <- 
  dat_acq2 %>% 
  {glm(acquisition ~ originDate, data = ., family = "binomial")}

# plot it

p_logit <- 
  dat_acq2 %>%
  mutate(acquisition = as.numeric(acquisition) %>% `-`(1)) %>% #pull(acquisition)
  ggplot(aes(x = originDate,
             y = acquisition, #%>% as.numeric() %>% `-`(1),
             colour = acquisition)) +# %>% as.character() %>% as_factor())) +
  # geom_point() +
  geom_linerange(aes(ymin = as.numeric(acquisition) - half_width*.2,
                     ymax = as.numeric(acquisition) + half_width*.2),
                 lwd = .2) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              colour = sc_pal2[3]) +
  ggplot2::annotate(x = as_date(as_date("2004-01-01")),
                    y = .5,
                    geom = "label",
                    label = "estimated probability\n(logistic regression)",
                    fill = sc_pal2[3],
                    colour = "white") +
  scale_color_continuous(low = sc_pal2[1],
                         high = sc_pal2[2]) +
  guides(colour = FALSE) +
  theme_bw() +
  theme(aspect.ratio = .28,
        plot.margin = margin(t = 0, r = 10, b = 10, l = 3, unit = "mm"),
        text = element_text(family = "Arial Narrow",
                            colour = "grey40")) +
  labs(x = "Day of Acquisiton",
       y = "PROBABILITY\nthat new Cetacean\nwas NOT Captured",
       caption = "Sources: FOIA, Ceta-Base; collected by Amber Thomas | Plot by @othomn")

p_logit
 
# save new plot with logistic regression
png(filename = "plots/38-cetaceans-logit.png",
    height = 2000, width = 2200,
    res = 300)
grid.newpage()
gtable_rbind(p_dens %>% ggplotGrob(),
             p_logit %>% ggplotGrob(),
             size = "max") %>% 
  grid.draw()
dev.off() 
