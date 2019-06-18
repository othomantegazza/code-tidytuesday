library(tidyverse)
library(ggforce)
# library(emojifont)
library(showtext)
library(grid)

font_families()
font_add(family = 'FontAwesome', regular = 'data/Font Awesome 5 Free-Solid-900.otf')
font_add_google("B612 Mono", family = "courier")

purple <- "#AA2255"
purple2 <- "#BB2255"
blue <-  "#4C63C3"
bg_col <- "#F0F0CB"
bg_col <- "#EAEA9F"

# get data ----------------------------------------------------------------


data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-06-18/bird_counts.csv")

data_path <- "data/2-25-birds.Rdata"

if(!file.exists(data_path)) {
  birds <- 
    data_url %>% 
    read_csv()
  
  save(birds, file = data_path)
} else {
  load(data_path)
}

# explore -----------------------------------------------------------------

birds %>% 
  count(year) %>% View()

birds %>% 
  count(species) %>% View()

birds %>% summary

birds %>% 
  ggplot(aes(x = total_hours)) +
  geom_histogram()


# prepare data for plot ---------------------------------------------------


med_20 <- 
  birds %>% 
  filter(year > 1997,
         year <= 2007) %>% 
  drop_na() %>% # no need
  group_by(species) %>% 
  summarise(count_20 = median(how_many_counted_by_hour))


med_10 <- 
  birds %>% 
  filter(year > 2007,
         year <= 2017) %>% 
  drop_na() %>% # no need
  group_by(species) %>% 
  summarise(count_10 = median(how_many_counted_by_hour))

to_plot <- 
  full_join(med_20, med_10) %>% 
  filter(!{count_20 < .007 & count_10 < .007})

# Plot --------------------------------------------------------------------


showtext_auto()
font_families()

p <- 
  to_plot %>% 
  ggplot(aes(x = reorder(species, count_20), 
             y = count_10)) +
  # ggimage::geom_image(image = bird_fa, size = .015) +
  # geom_text(label = "dove")
  geom_link(aes(xend = reorder(species, count_10),
                y = count_20,
                yend = count_10,
                size = ..index..,
                alpha = ..index..),
            colour = blue) +
  # geom_point(colour = "red") +
  # geom_text(label = fontawesome("fa-github"), family = "fontawesome-webfont") +
  geom_text(label = "\uf4ba", family = "FontAwesome",
            colour = purple,
            size = 6) +
  # geom_fontawesome() +
  coord_flip() +
  scale_y_log10() +
  scale_size_continuous(range = c(.1, .6)) +
  scale_alpha_continuous(range = c(.6, 1)) +
  guides(size = FALSE,
         alpha = FALSE) +
  theme_minimal() +
  labs(y = "Counts per hour",
       x = "",
       caption = "Data by Bird Studies Canada and Sharleen W. | Plot by @othomn") +
  theme(text = element_text(family = "courier"),
        title = element_text(face = "bold"),
        axis.title = element_text(hjust = 1, size = 9,
                                  colour = "grey10"),
        axis.text = element_text(hjust = 1, size = 10,
                                 colour = "grey10",
                                 family = "courier"),
        plot.caption = element_text(colour = purple, 
                                    # colour = "grey10",
                                    size = 8),
        plot.title = element_text(lineheight = .2),
        plot.subtitle = element_text(colour = purple,
                                     lineheight = .3),
        plot.margin = margin(14,2,2,2, unit = "mm"),
        panel.background =  element_rect(fill = bg_col, colour = bg_col),
        panel.grid = element_line(colour = "white", size = .15))

# p


# legend ------------------------------------------------------------------

dat_p_legend <- 
  tibble(y = c(.25, .75),
         label = c("Median\n1998 - 2007", "Median\n2008 - 2017"),
         xend = .85,
         curvature = c(.2, -.2)) %>% 
  mutate(yend = y + c(-.05, .05),
         y2 = y + c(-.02, .02)) 

geom_curve_2 <- function(x, xend, y, y2, yend, label, curvature) {
  dat <- tibble(xend = xend, y2 = y2, yend = yend, curvature = curvature)
  geom_curve(data = dat,
             aes(x = .96, 
                 xend = xend, 
                 y = y2, 
                 yend = yend),
             curvature = curvature,
             colour = "grey50",
             arrow = arrow(length = unit(.3, "mm"),
                           ends = "last",
                           type = "open"),
             size = .18) 
}

p_legend <- 
  dat_p_legend %>% 
  ggplot() +
  geom_text(aes(x = .78, y = yend,
                label = label,
                hjust = .5),
            family = "courier",
            size = 4,
            lineheight = .18) +
  geom_link(aes(x = 1, y = .25,
            xend = 1, yend = .75,
            alpha = ..index..,
            size = ..index..),
            colour = blue) +
  pmap(.l = dat_p_legend, .f = geom_curve_2) +
  geom_text(aes(x = 1.015, y = .75),
            label = "\uf4ba", family = "FontAwesome",
            colour = purple,
            size = 10) +
  scale_size_continuous(range = c(.2, 1)) +
  lims(x = c(.6, 1.2),
       y = c(0, 1)) +
  guides(size = FALSE,
         alpha = FALSE) +
  coord_flip() +
  theme_void()


p_legend

# save plot ---------------------------------------------------------------



png(filename = "plots/2-25-birds.png",
    res = 500,
    height = 1946,
    width = 800)
grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
p %>% print(vp = viewport())
p_legend %>% print(vp = viewport(x = .6, y = .9,
                                 width = .5, height = .1))

grid.text(label = str_wrap("Christmas Bird Counts in the Hamilton Area of Ontario",
                           width = 40),
          vjust = .5,
          hjust = .5,
          x = .5,
          y = .97, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 18,
                    col = "#7A82A6",
                    lineheight = .2))
dev.off()

svg(filename = "plots/2-25-birds.svg",
    width = 6)
p %>% print()
dev.off()


birds %>% 
  filter(species %>% str_detect("winged Scoter")) %>% View()

birds %>% 
  filter(species %>% str_detect("Northern Pintail")) %>% View()
