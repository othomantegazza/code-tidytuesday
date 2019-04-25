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
         count)

edges <- 
  dat_graph %>% 
  filter(x != y)

g <- graph_from_data_frame(d = edges)


# plot with ggraph --------------------------------------------------------

library(ggraph)

# g %>% as_long_data_frame()

g %>% 
  ggraph(layout = "linear") +
  geom_edge_arc(aes(alpha = count)) +
  theme_void()
