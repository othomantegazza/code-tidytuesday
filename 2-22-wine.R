library(tidyverse)
library(broom)

wine_purple <- "#AA2255"

# Get data ----------------------------------------------------------------

data_url <- paste0("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-05-28/winemag-data-130k-v2.csv?raw=true")

data_path <- "data/2-22-wine.Rdata"

if(!file.exists(data_path)) {
 wine <- 
   data_url %>% 
   read_csv()
  
  save(wine, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

wine %>% 
  filter(country == "Italy") %>% 
  ggplot(aes(x = reorder(province, price, order = T),
             y = price)) +
  ggbeeswarm::geom_quasirandom(alpha = .2) +
  coord_flip() +
  scale_y_log10()

wine %>% pull(region_1) %>% unique()

wine %>% 
  ggplot(aes(x = points,
             y = price)) +
  geom_hex(bins = 20, colour = "white") +
  scale_y_log10()

wine %>% 
  filter(province == "Lombardy") %>% # distinct(variety)
  ggplot(aes(x = points %>% as.character(),
             y = price,
             fill = variety)) +
  geom_boxplot()+
  facet_wrap(facets = "variety")



#  test some model --------------------------------------------------------

set.seed(46)

wine_ita <- 
  wine %>% 
  filter(country == "Italy") %>% 
  mutate(test = sample(c("train", "test"),
                       size = nrow(.),
                       replace = T)) %>% 
  drop_na(points, price)

train_test <- function(n, dat) {
  dat_train <- 
    dat %>% 
    filter(test == "train") 
  
  dat_test <- 
    dat %>% 
    filter(test == "test")
  
  fit_train <- 
    dat_train %>% 
    {lm(price ~ poly(points, n), data = dat)}
  
  MSE_train <- 
    dat_train %>% 
    {broom::augment(fit_train, newdata = .)} %>% 
    mutate(SE = (.fitted - price)^2) %>% 
    pull(SE) %>% mean()
  

  MSE_test <- 
    dat_test %>% 
    {broom::augment(fit_train, newdata = .)} %>% 
    mutate(SE = (.fitted - price)^2) %>% 
    pull(SE) %>% mean()
  
  tibble(mse_train = MSE_train,
         mse_test = MSE_test)
}

ploy_res_ita <- 
  1:20%>% 
  map_df(~train_test(., dat = wine_ita),
         .id = "poly_degree")



wine %>% 
  filter(country == "Italy") %>%
  {lm(price ~ poly(points, 16), data = .)} %>% 
  summary()


# plot italian wines ---------------------------------------------------------------

# 
# add_count(variety) %>%
#   filter(n > 300) %>% 
#   filter(!str_detect(variety, "Blend")) %>% 

wine %>% 
  filter(country == "Italy") %>%
  # add_count(variety) %>%
  # filter(n > 300) %>% 
  # filter(!str_detect(variety, "Blend")) %>% 
  ggplot(aes(x = points, 
             y = price)) +
  geom_hex(bins = 20, colour = "white", size = 1) +
  geom_smooth(formula = y ~ poly(x, 5),
              method = lm,
              size = 2) +
  # facet_wrap(facets = "variety") +
  # scale_y_log10() +
  scale_fill_viridis_c(option = "C") +
  theme_bw()

annos_tb <- 
  tibble(points = c(79.9, 96, 97),
         price = c(40, 10, 38),
         xend = c(80.5, 95, 96.2),
         yend = c(27, 23, 27),
         colourz = c("black", "red", "red"))


best_95 <- 
  wine_ita %>% 
  filter(points == 95, 
         price == 25) %>% 
  pull(title)

best_96 <- 
  wine_ita %>% 
  filter(points == 96, 
         price == 27) %>% 
  pull(title)



p <- 
  wine_ita %>% 
  ggplot(aes(x = points, 
             y = price,
             group = points)) +
  ggbeeswarm::geom_quasirandom(alpha = .5,
                               colour = wine_purple,
                               size = .8) +
  geom_hline(yintercept = 25) +
  geom_curve(data = annos_tb,
             aes(xend = xend,
                 yend = yend),
             curvature = -.3,
             arrow = arrow(length = unit(3, "mm"),
                           ends = "last",
                           type = "open")) +
  # annotate 25 line
  annotate(geom = "text",
           x = 79.8, y = 40,
           label = "25$",
           vjust = .5, hjust = 1,
           family = "Courier") +
  # annotate best choice at 95 points
  annotate(geom = "text",
           x = 96.2, y = 10,
           label = best_95 %>% str_wrap(15),
           vjust = .5, hjust = 0,
           family = "Courier",
           fontface = "bold",
           size = 3,
           lineheight = 1,
           colour = wine_purple) +
  annotate(geom = "text",
           x = 96.5, y = 40,
           label = best_96 %>% str_wrap(15),
           vjust = 0, hjust = 0,
           family = "Courier",
           fontface = "bold",
           size = 3,
           lineheight = 1,
           colour = wine_purple) +
  ylim(0, 100) +
  labs(x = "Review scores by Vivino [range 80 - 100]",
       y = "Price [USD]",
       caption = "Data at Kaggle | Plot by @othomn",
       title = "Price of Italian Wine by Score\n",
       subtitle = "According to Wine-Enthusiast. Only Italian wines priced less than 100 $ are shown.\n") +
  theme_minimal() +
  theme(text = element_text(family = "Courier"),
        title = element_text(face = "bold"),
        axis.title = element_text(hjust = 1, size = 10,
                                  colour = "grey10"),
        plot.caption = element_text(colour = wine_purple, 
                                    # colour = "grey10",
                                    size = 8),
        plot.title = element_text(lineheight = .2),
        plot.subtitle = element_text(colour = wine_purple,
                                     lineheight = .3),
        plot.margin = margin(5,4,4,2, unit = "mm"))


png(filename = "plots/2-22-wine.png",
    res = 300,
    height = 1600,
    width = 2600)
p %>% print()
dev.off()
