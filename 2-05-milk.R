library(tidyverse)
library(scico)
library(tibbletime)

# Get data Milk products ---------------------------------------------------

dat_path <- "data/2-05-milk-product-facts.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-29/milk_products_facts.csv")

if(!file.exists(dat_path)) {
  dat_milkprods <- 
    read_csv(dat_url)
  
  save(dat_milkprods, file = dat_path)
  
} else {
  load(dat_path)
}


# Percent ---------------------------------------------------------

# roll over tibbles
roll_percent <- rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)

dat <- 
  dat_milkprods %>%
  select(year, butter) %>% 
  mutate(percent = roll_percent(butter)) %>% 
  filter(complete.cases(.))


# plot --------------------------------------------------------------------

# needed to center divergent palette
lim <- 
  dat$percent %>% 
  range() %>% 
  abs() %>% 
  max()


p <- 
  dat %>% 
  mutate(yend = butter + (percent/10)) %>% 
  ggplot(aes(x = year,
             y = butter)) +
  annotate(geom = "rect",
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = .5) +
  annotate(geom = "text",
           x = 2009, y = 4,
           label = "2008\nEconomic Crisis?",
           family = "Arial Narrow",
           colour = "grey40",
           size = 3, fontface = "bold") +
  # geom_line(color = "grey80") +
  geom_segment(aes(yend = yend,
                   xend = ..x..,
                   colour = percent),
               size = 2,
               arrow = arrow(length = unit(1.2, "mm"),
                             type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  geom_text(aes(y = case_when(percent > 0 ~ yend + .12,
                              TRUE ~ yend - .12),
                label = percent %>% 
                  round() %>% paste0("%"),
                colour = percent),
            size = 2.7) +
  scale_colour_scico(palette = "roma",
                   direction = 1,
                   limits = c(-lim, lim),
                   guide = FALSE) +
  guides(colour = element_blank()) +
  labs(title = "Fluctuations in Butter Consumptions",
       subtitle = str_wrap("In the US between 1975 - 2017,
                           with weight of sold butter in lbs 
                           and its percent change compared to
                           the previous year."),
       y = "Sold Butter in lbs",
       x = "Year",
       caption = "Data: USDA | Plot by @othomn") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow",
                            colour = "grey40",
                            size = 11),
        axis.title = element_text(size = 14),
        plot.title = element_text(colour = "grey20",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(face = "bold",
                                     size = 12),
        aspect.ratio = .6,   
        plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                             unit = "mm"))

p
# Save --------------------------------------------------------------------

png(filename = "plots/2-05-milk.png",
    height = 1600, width = 2100,
    res = 300)
p %>% print()
dev.off() 

