library(tidyverse)

# Get data ---------------------------------------------------

rd_path <- "data/2-07-funding.Rdata"
rd_url <- paste0("https://raw.githubusercontent.com/",
                 "rfordatascience/tidytuesday/master/data/",
                 "2019/2019-02-12/fed_r_d_spending.csv")

if(!file.exists(rd_path)) {
  rd <- 
    read_csv(rd_url)
  
  save(rd, file = rd_path)
  
} else {
  load(rd_path)
}

# explore -----------------------------------------------------------------

# Try some cluster --------------------------------------------------------

# hierarchical
rd_wide_01 <- 
  rd_wide %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, ~scales::rescale(., to = c(0,1))) %>% 
  column_to_rownames() %>% 
  t()

rd_wide_01 %>%  
  dist() %>%
  hclust() %>%
  plot()

# kmeans
k_clust <- 
  rd_wide_01 %>%
  kmeans(centers = 2)

clust_df <- 
  k_clust$cluster %>% 
  {tibble(department = names(.),
          cluster = .)}

# plot
p <- 
  k_clust %>%
  broom::augment(data = rd_wide_01)  %>% 
  rename_all(~str_sub(., 2, -1)) %>% 
  gather(`1976`:`2017`, key = "year", value = "scaled_funding") %>% 
  mutate(cluster = cluster %>% as.character()) %>% 
  ggplot(aes(x = year,
             y = rownames,
             fill = scaled_funding)) +
  geom_tile() +
  facet_grid(cluster ~ ., scales = "free", space = "free") +
  scale_fill_viridis_c(option = "D",
                       breaks = c(0, .2, .4, .6, .8, 1),
                       guide = guide_legend(label.position = "top", 
                                            keyheight = unit(2, units = "mm"),
                                            keywidth=unit(15, units = "mm"), 
                                            nrow = 1,
                                            title.vjust = 0, 
                                            title.theme = element_text(family = "Arial Narrow",
                                                                       colour = "grey40",
                                                                       size = 12.5,
                                                                       face = "bold"))) +
  labs(title = "US Federal Research and Development Spending by Agency",
       subtitle = str_wrap("Relative patterns of spending split in two clusters [k-means
                           and hierarchical algorithms have similar results].
                           Fundings for group 1 increase sharply in the
                           late '90s and '00s, fundings for group 2 falls sharply in
                           the '80s an then stabilizes.",
                           width = 90),
       y = "",
       fill = "Scaled Spending*", 
       x = "Year",
       caption = paste0("*Absolute spending (USD) is adjusted for inflation, ",
                        "then every row is scaled between 0 and 1 ",
                        "to highlight relative patterns.\n",
                        "Source: AAAS | Plot by @othomn")) +
  scale_x_discrete(labels = function(lbs) {
    lbs_num <- lbs %>% as.numeric()
    print(lbs)
    lbs_num <- case_when(lbs_num %% 5 == 0 ~ lbs_num,
                   TRUE ~ 0) %>%
      as.character()
    out <- case_when(lbs_num != "0" ~ lbs_num,
                     TRUE ~ "")
    # lbs %>% as.numeric()
    }) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "top",
        text = element_text(family = "Arial Narrow",
                            colour = "grey40",
                            size = 11),
        axis.title = element_text(size = 14, face = "bold"),
        axis.ticks.x = element_line(colour = "grey70"),
        plot.title = element_text(colour = "grey20",
                                  face = "bold",
                                  size = 16),
        # strip.text.y = element_text(angle = 0,
        #                             size = 9,
        #                             hjust = 0),
        strip.background = element_rect(fill = "grey90",
                                        colour = "grey90"),
        plot.subtitle = element_text(size = 12), 
        plot.margin = margin(t = 5, r = 10, b = 0, l = 10,
                             unit = "mm")) 


# p                


# save --------------------------------------------------------------------

png(filename = "plots/2-07-funding-heat.png",
    height = 1600, width = 2100,
    res = 300)
p %>% print()
dev.off() 

