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
           as.character(),
         # Set 0 to NA, better for percent changes
         scaled_funding = scaled_funding %>% 
           {case_when(. == 0 ~ NA_real_,
                     TRUE ~ .)}) 
  
# roll percent change
roll_percent <- rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)

clust_percent <- 
  clust_tidy %>% 
  arrange(agency, year) %>% 
  group_by(agency) %>% 
  mutate(percent = roll_percent(scaled_funding)) #%>% View()


# plot --------------------------------------------------------------------

clust_percent %>% 
  ggplot(aes(x = year,
             y = percent,
             fill = percent)) +
  geom_bar(stat = "identity") +
  facet_grid(agency ~ ., scales = "free_y")
