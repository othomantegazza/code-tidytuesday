library(tidyverse)

# Get data Milk products ---------------------------------------------------

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


# set theme ---------------------------------------------------------------

theme_set(theme_bw() +
            theme(axis.text.x = element_text(angle = 90, vjust = .5)))

# explore -----------------------------------------------------------------

# rd %>% View()

rd %>% 
  ggplot(aes(x = year %>% as.character %>% as_factor(),
             y = rd_budget)) +
  geom_boxplot()

rd %>% 
  ggplot(aes(x = year,
             y = rd_budget,
             group = department)) +
  geom_line() +
  scale_y_log10()

# correlation
library(ggfortify)
library(GGally)

rd_wide <- 
  rd %>% 
  select(department, year, rd_budget) %>% 
  tidyr::spread(key = year,
                value = rd_budget) %>%
  column_to_rownames("department") %>%
  t()
  
# rd_wide %>% View()

cors <- 
  rd_wide %>% 
  cor() 

cors %>% GGally::ggcorr()
cors %>% 
  autoplot() +
  scale_fill_viridis_c(option = "E")

# cors %>% GGally::ggpairs()


# Try some cluster --------------------------------------------------------

# hierarchical
rd_wide %>% scale() %>% t() %>% #View()
  dist() %>%  hclust() %>% plot()

# kmeans
k_clust <- 
  rd_wide %>%
  scale() %>%
  t() %>%
  kmeans(centers = 2)

clust_df <- 
  k_clust$cluster %>% 
  {tibble(department = names(.),
          cluster = .)}

# plot
k_clust %>%
  broom::augment(data = rd_wide %>% scale() %>% t()) %>% 
  rename_all(~str_sub(., 2, -1)) %>% 
  gather(`1976`:`2017`, key = "year", value = "scaled_funding") %>% 
  mutate(cluster = cluster %>% as.character()) %>% 
  ggplot(aes(x = year,
             y = scaled_funding,
             colour = cluster)) +
  geom_line(aes(group = rownames),
            alpha = .5) +
  # geom_smooth() +
  stat_summary(aes(group = cluster),
               geom = "line",
               fun.y = median,
               size = 2) +
  facet_grid(cluster ~ .)

# plot reaal values
rd %>% 
  left_join(clust_df, by = "department") %>% 
  ggplot(aes(x = year,
             y = rd_budget,
             colour = cluster)) + 
  geom_line() + 
  facet_wrap("department", 
             scales = "free_y")


# Obama years + Trump begin -----------------------------------------------

rd_last <- 
  rd %>% 
  filter(year >= 2009) 
  
rd_last_wide <- 
  rd_last %>% 
  select(department, year, rd_budget) %>% 
  tidyr::spread(key = year,
                value = rd_budget) %>%
  column_to_rownames("department") %>%
  t()

rd_last_wide %>% 
  cor(method = "spearman") %>% 
  autoplot() +
  scale_fill_viridis_c()

rd_last_wide %>% scale() %>% t() %>% #View()
  dist() %>%  hclust() %>% plot()

# kmeans
last_k_clust <- 
  rd_last_wide %>%
  scale() %>%
  t() %>%
  kmeans(centers = 2)

last_clust_df <- 
  last_k_clust$cluster %>% 
  {tibble(department = names(.),
          cluster = .)}

# plot
rd_last %>% 
  left_join(last_clust_df, by = "department") %>% 
  ggplot(aes(x = year,
             y = rd_budget,
             colour = cluster)) + 
  geom_line() + 
  facet_wrap("department", 
             scales = "free_y")

# plot scaled values
last_k_clust %>%
  broom::augment(data = rd_last_wide %>% scale() %>% t()) %>% 
  rename_all(~str_sub(., 2, -1)) %>% 
  gather(`2009`:`2017`, key = "year", value = "scaled_funding") %>% 
  mutate(cluster = cluster %>% as.character()) %>% 
  ggplot(aes(x = year,
             y = scaled_funding,
             colour = cluster)) +
  geom_line(aes(group = rownames),
            alpha = .5) +
  # geom_smooth() +
  stat_summary(aes(group = cluster),
               geom = "line",
               fun.y = median,
               size = 2) +
  facet_grid(cluster ~ .)
