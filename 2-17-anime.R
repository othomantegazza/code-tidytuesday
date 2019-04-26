library(tidyverse)

# get data ----------------------------------------------------------------

anime_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                "tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
anime_file <- "data/2-17-anime.Rdata"


if(!file.exists(anime_file)) {
  anime <- readr::read_csv(anime_url)
  
  save(anime, file = anime_file)
  
} else {
  load(anime_file)
}


# Explore -----------------------------------------------------------------

anime$producers %>% table() %>% sort(decreasing = T) %>% head()
anime$genre %>% table() %>% sort(decreasing = T) #%>% head()

to_plot <- 
  with(anime, table(genre, producers)) %>%
  as_tibble() %>%
  arrange(desc(n))



# graph ? -----------------------------------------------------------------

dat_graph <- 
  anime %>% 
  select(animeID, genre) %>% 
  filter(complete.cases(.)) %>% 
  distinct() %>% 
  split(.$animeID) %>% 
  map(~expand.grid(x = .$genre, y = .$genre) %>% 
        as_tibble() %>% 
        mutate_all(as.character)) %>% 
  reduce(bind_rows) %>% 
  mutate(count = 1) %>% 
  group_by(x, y) %>% 
  summarise(count = sum(count))

dat_graph %>% filter(x == y)


# with igraph -------------------------------------------------------------

library("igraph")

vs <- 
  dat_graph %>%
  filter(x == y) %>% 
  select(genre = x,
         count) %>% 
  filter(count >= 500)

edges <- 
  dat_graph %>% 
  mutate(n_x = case_when(x == y ~ count)) %>% 
  arrange(x, n_x) %>% 
  fill(n_x) %>% 
  mutate(perc = count/n_x) %>% 
  filter(perc > .2,
         x %in% vs$genre,
         y %in% vs$genre) %>% 
  filter(x != y)

g <- graph_from_data_frame(d = edges, vertices = vs)


# plot with ggraph --------------------------------------------------------

library(ggraph)


g %>% as_long_data_frame()

set.seed(7)
# set.seed(12)
# set.seed(16)
p <- 
  g %>% 
  ggraph(layout = "graphopt") +
  # ggraph(layout = "lgl") +
  geom_edge_link(aes(alpha = count),
                 colour = "#4C63C3") +
  geom_node_point(aes(size = count), colour = "#B63A82") +
  geom_node_text(aes(label = name),
                 nudge_x = 1,
                 nudge_y = 1,
                 vjust = 0,
                 hjust = 0) +
  # geom_text(aes(label = count)) +
  expand_limits(x = c(0, 50)) +
  theme_void()

p %>% ggplotGrob()

p +
  guides(alpha = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1)) +
  theme(legend.position = c(.7, .3))

# Save --------------------------------------------------------------------

png(filename = "plots/2-17-anime.png", 
    height = 7,
    width = 10,
    units = "in",
    res = 300)
p
dev.off()

# others ------------------------------------------------------------------



g %>% 
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(alpha = count),
                 colour = "#4C63C3") +
  geom_node_point(aes(size = count), colour = "#B63A82") +
  geom_node_text(aes(label = name),
                 angle = 90) +
  # geom_text(aes(label = count)) +
  theme_void()

g %>% 
  ggraph(layout = "linear") +
  geom_edge_arc(aes(alpha = count)) +
  theme_void()

g %>% 
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc2(aes(alpha = count)) +
  theme_void()

# c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
#   'randomly', 'fr', 'kk', 'drl', 'lgl')