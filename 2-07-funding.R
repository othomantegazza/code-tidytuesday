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
p <- 
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
p_last <- 
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


# fix plots ---------------------------------------------------------------


title_blue <- "#5F3BBA"

depts <- c(
  DOD = "Deparment of Defense",
  NASA = "National Aeronautics and Space Administration",
  DOE = "Department of Energy",
  HHS = "Department of Health and Human Services",
  NIH = "National Institute of Health",
  NSF = "National Science Foundation",
  USDA = "US Department of Agriculture",
  Interior = "Department of Interior",
  DOT = "Deparment of Transportation",
  EPA = "Environmental Protection Agency",
  DOC = "Department of Corrections",
  DHS = "Department of Homeland Security",
  VA = "Department of Veterands Affairs",
  Other = "other R&D spending")

cl_depts <- 
  clust_df %>% 
  mutate(full_name = depts[department] %>% unname()) %>% 
  {split(.$full_name, .$cluster)} %>% 
  map_chr(~paste(., collapse = "\n")) 

  
# plot
p <- 
  k_clust %>%
  broom::augment(data = rd_wide %>% scale() %>% t()) %>% 
  rename_all(~str_sub(., 2, -1)) %>% 
  gather(`1976`:`2017`, key = "year", value = "scaled_funding") %>% 
  mutate(cluster = cluster %>% as.character()) %>% 
  ggplot(aes(x = year %>% as.numeric(),
             y = scaled_funding,
             colour = cluster)) +
  geom_line(aes(group = rownames),
            alpha = .5) +
  # geom_smooth() +
  stat_summary(aes(group = cluster),
               geom = "line",
               fun.y = median,
               size = 2) +
  facet_grid(cluster ~ .,
             labeller = labeller(cluster = cl_depts)) +
  scale_color_viridis_d(begin = .3, end = .8,
                        option = "B", guide = FALSE) +
  labs(title = "US Federal Research and Development Spending by Agency",
       subtitle = str_wrap("Spending splits in two clusters [k-means
                           and hierarchical algorithms have similar results].
                           Fundings for group 1 increase sharply in the
                           late '90s and '00s, fundings for group 2 falls sharply in
                           the '80s an then stabilizes."),
       y = "Z-score normalized spendings*",
       x = "Year",
       caption = "*Adjusted for inflation\nSource: AAAS | Plot by @othomn") +
  theme_bw() +
  theme(text = element_text(family = "Arial Narrow",
                            colour = "grey40",
                            size = 11),
        axis.title = element_text(size = 14),
        plot.title = element_text(colour = "grey20",
                                  face = "bold",
                                  size = 16),
        strip.text.y = element_text(angle = 0,
                                    size = 9,
                                    hjust = 0),
        strip.background = element_rect(fill = "grey90",
                                        colour = "grey90"),
        plot.subtitle = element_text(face = "bold",
                                     size = 12),
        aspect.ratio = .4,   
        plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                             unit = "mm"),
        plot.caption = element_text(hjust = 0)) 

# p


# Save --------------------------------------------------------------------

png(filename = "plots/2-07-funding.png",
    height = 1600, width = 2100,
    res = 300)
p %>% print()
dev.off() 
