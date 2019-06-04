library(tidyverse)
library(ggwordcloud)
library(grid)
library(ggforce)

purple <- "#AA2255"
purple2 <- "#BB2255"

# Get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/",
                   "data/2019/2019-06-04/ramen_ratings.csv")

data_path <- "data/2-23-ramen.Rdata"

if(!file.exists(data_path)) {
  ramen <- 
    data_url %>% 
    read_csv()
  
  save(ramen, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

# ramen %>% View()

ramen %>%  
  count(brand) %>% 
  arrange(desc(n))

ramen %>% 
  count(style) %>% 
  arrange(desc(n))

ramen %>% 
  count(country) %>% 
  arrange(desc(n))

ramen %>% 
  count(variety) %>% 
  arrange(desc(n))


ramen %>% 
  ggplot(aes(x = style,
             y = stars)) +
  geom_count()

# stars by style
ramen %>% 
  filter(!style %in% c("Bar", "Can", "Restaurant")) %>% 
  ggplot(aes(x = style,
             y = stars)) +
  geom_boxplot() +
  geom_count()


# stars by country
top_c <-
  ramen %>% 
  count(country) %>%
  arrange(desc(n)) %>% 
  top_n(n = 10) %>% 
  pull(country)

third_q <- function(x) quantile(x, probs = 3/4)
first_q <- function(x) quantile(x, probs = 1/4)


purple <- "#CD0A30"

p <- 
  ramen %>% 
  filter(country %in% top_c) %>% 
  select(country, stars) %>% 
  drop_na() %>% 
  mutate(country = reorder(country, stars, FUN = "median")) %>% 
  ggplot(aes(x = country,
             y = stars)) +
  geom_count(aes(alpha = ..n..), colour = purple) +
  stat_summary(aes(x = as.numeric(country) + .3),
               fun.y = median,
               fun.ymax = third_q,
               fun.ymin = first_q) +
  coord_flip() +
  theme_minimal() +
  scale_alpha_continuous(range = c(.5, 1)) +
  guides(size = guide_legend(nrow = 1),
         alpha = FALSE) +
  labs(x = "Country",
       y = "Star Rating",
       size = "Count",
       caption = "Data from theramenrater.com | Plot by @othomn") +
  theme(text = element_text(family = "courier"),
        title = element_text(face = "bold"),
        axis.title = element_text(hjust = 1, size = 10,
                                  colour = "grey10"),
        plot.caption = element_text(colour = purple, 
                                    # colour = "grey10",
                                    size = 8),
        plot.title = element_text(lineheight = .2),
        plot.subtitle = element_text(colour = purple,
                                     lineheight = .3),
        plot.margin = margin(5,4,4,2, unit = "mm"),
        legend.position = "left",
        legend.justification = "center", 
        legend.margin = margin(0, 10, 0, 40, unit = "mm"))


dat_p_legend <- 
  tibble(y = c(.25, .5, .75),
         label = c("1st quartile", "median", "3rd quartile"),
         xend = .85,
         curvature = c(.2, 0, -.2)) %>% 
  mutate(yend = y + c(-.05, 0, .05),
         y2 = y + c(-.02, 0, .02)) 

geom_curve_2 <- function(x, xend, y, y2, yend, label, curvature) {
  dat <- tibble(xend = xend, y2 = y2, yend = yend, curvature = curvature)
  geom_curve(data = dat,
             aes(x = .96, 
                 xend = xend, 
                 y = y2, 
                 yend = yend),
             curvature = curvature,
             colour = purple,
             arrow = arrow(length = unit(3, "mm"),
                           ends = "last",
                           type = "open")) 
}

p_legend <- 
  dat_p_legend %>% 
  ggplot() +
  geom_text(aes(x = .8, y = y,
                label = label,
                hjust = 1- y),
            family = "courier") +
  geom_pointrange(x = 1, y = .5,
                 ymin = .25, ymax = .75) +
  pmap(.l = dat_p_legend, .f = geom_curve_2) +
  lims(x = c(0, 2),
       y = c(0, 1)) +
  coord_flip() +
  theme_void()
  

png(filename = "plots/2-23-ramen.png",
    width = 3500,
    height = 1600,
    res = 300)
p 
grid.lines(x = unit(c(.355, .355), "npc"),
           y = unit(c(.18, .95), "npc"))
grid.text(label = str_wrap("Ramen Rating by Country"),
          vjust = 1,
          hjust = 1,
          x = .34,
          y = .95, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 14,
                    col = purple))
grid.text(label = str_wrap("All ramens rated by TheRamenRater:
                           a one man initiative and top authority in the field.
                           Respect! :)",
                           width = 30),
          vjust = 1,
          hjust = 1,
          x = .34,
          y = .85, 
          gp = gpar(fontfamily = "courier",
                    fontsize = 10,
                    lineheight = .84))
print(p_legend, vp = viewport(x = .2, y = .33, width = .3))
dev.off()


# model -------------------------------------------------------------------

# words -------------------------------------------------------------------
ramen_w <- 
  ramen %>% 
  mutate(variety = variety %>% str_split(" ")) %>%
  split(.$country) %>% 
  map_df(~pull(., variety) %>%
           reduce(., c) %>%
           {tibble(variety = .)} %>% 
           count(variety),
         .id = "country")

ramen_w %>% 
  filter(country == "Malaysia") %>% 
  ggplot(aes(label = variety, size = n)) +
  geom_text_wordcloud()


ramen_w %>% 
  filter(country == "United States") %>% 
  ggplot(aes(label = variety, size = n)) +
  geom_text_wordcloud()
