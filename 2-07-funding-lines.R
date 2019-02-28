library(tidyverse)
library(tibbletime)

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

rd_wide <- 
  rd %>% 
  select(department, year, rd_budget) %>% 
  tidyr::spread(key = year,
                value = rd_budget) %>%
  column_to_rownames("department") %>%
  t()

# Try some cluster --------------------------------------------------------

# hierarchical
rd_wide_01 <- 
  rd_wide %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, ~scales::rescale(., to = c(0,1))) %>%
  # mutate_if(is.numeric, scale)
  column_to_rownames() %>% 
  t()


rd_hclust <- 
  rd_wide_01 %>%  
  dist() %>%
  hclust() 

rd_hclust %>% plot()

# ordered factor

# kmeans
k_clust <- 
  rd_wide_01 %>%
  kmeans(centers = 2)

clust_df <- 
  k_clust$cluster %>% 
  {tibble(department = names(.),
          cluster = .)}

# Tidy clusters
clust_tidy <- 
  k_clust %>%
  broom::augment(data = rd_wide_01)  %>% 
  rename_all(~str_sub(., 2, -1)) %>% 
  gather(`1976`:`2017`,
         key = "year",
         value = "scaled_funding") %>% 
  rename(agency = "rownames") %>% 
  mutate(cluster = cluster %>%
           as.character()) %>% 
  mutate(agency = agency %>% 
           factor(levels = rd_hclust$labels[rd_hclust$order]))
  

# plot --------------------------------------------------------------------

pwidth <- 5
lwidth <- .5
scfill <- 
  scale_fill_viridis_c(
    option = "D",
    breaks = c(0, .2, .4, .6, .8, 1),
    guide = guide_legend(
      label.position = "top", 
      keyheight = unit(2, units = "mm"),
      keywidth=unit(15, units = "mm"), 
      nrow = 1,
      title.vjust = 0, 
      title.theme = element_text(family = "Arial Narrow",
                                 colour = "grey40",
                                 size = 12.5,
                                 face = "bold"))
  ) 


p <- 
  clust_tidy %>% 
  ggplot(aes(x = year,
             y = scaled_funding,
             fill = scaled_funding)) +
  geom_bar(stat = "identity", width = .3) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_hline(yintercept = .2, colour = "white", size = lwidth) +
  geom_hline(yintercept = .4, colour = "white", size = lwidth) +
  geom_hline(yintercept = .6, colour = "white", size = lwidth) +
  geom_hline(yintercept = .8, colour = "white", size = lwidth) +
  facet_grid(agency ~ .) +
  theme_minimal() + 
  scale_fill_viridis_c(
    option = "D",
    breaks = c(0, .2, .4, .6, .8, 1),
    guide = guide_legend(
      label.position = "top", 
      keyheight = unit(2, units = "mm"),
      keywidth=unit(8, units = "mm"), 
      nrow = 1,
      title.vjust = 0, 
      title.theme = element_text(family = "Arial Narrow",
                                 colour = "grey40",
                                 size = 12.5,
                                 face = "bold"))
  ) + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

svglite::svglite(file = 'plots/2-07-funding-lines.svg',
                 width = pwidth)
p %>% print
dev.off()


# plot 2 ------------------------------------------------------------------

p2 <- 
  clust_tidy %>% 
  ggplot(aes(x = year,
             y = scaled_funding,
             fill = scaled_funding,
             colour = scaled_funding)) +
  geom_point(size = .5) +
  geom_bar(stat = "identity", width = .1) +
  geom_hline(yintercept = 0, colour = "grey80") +
  facet_grid(agency ~ .) +
  theme_minimal() + 
  scfill + 
  scale_colour_viridis_c(guide = FALSE) + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

svglite::svglite(file = 'plots/2-07-funding-lines2.svg',
                 width = 5)
p2 %>% print()
dev.off()


# p3 line size ------------------------------------------------------------

p3 <- 
  clust_tidy %>% 
  ggplot(aes(x = year,
             y = scaled_funding,
             fill = scaled_funding,
             colour = scaled_funding,
             group = agency)) +
  # geom_point(size = .5) +
  # geom_bar(stat = "identity", width = .1) +
  geom_ribbon(aes(size = scaled_funding)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  facet_grid(agency ~ .) +
  theme_minimal() + 
  scale_size(guide = FALSE) +
  scfill + 
  scale_colour_viridis_c(guide = FALSE) + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(colour = "grey40"),
        strip.text = element_text(colour = "grey40"))

p3

svglite::svglite(file = 'plots/2-07-funding-lines3.svg',
                 width = 5)
p3 %>% print()
dev.off()

png(filename = "plots/2-07-funding-lines3.png",
    height = 2000, width = 1400,
    res = 300)
p3 %>% print()
dev.off() 
