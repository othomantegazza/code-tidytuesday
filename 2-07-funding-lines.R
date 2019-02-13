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

lwidth = .5

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
  scale_fill_viridis_c() + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())

