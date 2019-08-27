library(tidyverse)
library(grid)


# get data ----------------------------------------------------------------

data_url <- paste0('https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
                   'master/data/2019/2019-08-27/simpsons-guests.csv')

data_path <- 'data/2-35-simpsons.Rdata'


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  simpsons <- 
     data_url %>% 
    read_delim(data_url, delim = "|")#, quote = "")# , col_integer()))
  
  # download.file(data_url, csv_path)
  
  # simpsons <- read_delim(csv_path, delim = "|", quote = "")
  
  save(simpsons, file = data_path)
} else {
  load(data_path)
}


# tidy data ---------------------------------------------------------------

simpsons2 <- 
  simpsons %>% 
  mutate(season = as.numeric(season)) %>% 
  separate(col = number, into = c("episode_count", "season_count"), sep = "–") %>% 
  mutate(episode_count = as.numeric(episode_count))


start_at <- 
  simpsons2 %>% 
  group_by(guest_star) %>% 
  summarise(ep_min = min(episode_count, na.rm = TRUE))
  # {set_names(x = .$ep_min,
  #            nm = .$guest_star)}

seasons <-
  simpsons3 %>% 
  group_by(season) %>% 
  summarise(ep_min_season = min(episode_count))

simpsons3 <- 
  simpsons2 %>%
  left_join(start_at) %>% 
  # left_join(seasons) %>% 
  mutate(y = episode_count - ep_min)
  

# explore -----------------------------------------------------------------

simpsons2 %>% 
  count(guest_star, sort = TRUE)



# plot --------------------------------------------------------------------

p2 <- simpsons3 %>% 
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev(),
             y = episode_count)) +
  geom_point(size = .1) +
  geom_line(aes(group = guest_star), size = .1) +
  coord_flip() +
  theme_void()

p <- simpsons3 %>% 
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev())) +
  geom_point(aes(y = y), size = .2) +
  geom_line(aes(group = guest_star,
                y = y), size = .2) +
  coord_flip() +
  theme_void()
 

# add season lines --------------------------------------------------------

seasons <-
  simpsons3 %>% 
  group_by(season) %>% 
  summarise(ep_max_season = max(episode_count)) %>% 
  drop_na

season_lines <- 
  expand.grid(guest_star = simpsons3 %>% pull(guest_star) %>% unique(),
              season = simpsons3 %>% pull(season) %>% unique()) %>% 
  as_tibble() %>% 
  left_join(start_at) %>% 
  left_join(seasons) %>% 
  mutate(season_at = ep_max_season - (ep_min - 2)) %>% 
  filter(season_at >= 0)

p_lines <- simpsons3 %>% 
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev())) +
  geom_point(aes(y = y), size = .2) +
    geom_line(data = season_lines,
              aes(y = season_at,
                  group = season,
                  colour = season),
              size = 4,
              alpha = .7) +
  geom_point(aes(y = y), size = .2) +
  geom_line(aes(group = guest_star,
                y = y), size = .2) +
  coord_flip() +
  guides(colour = FALSE,
         fill = FALSE) +
  theme_void()

p_lines

# print plot --------------------------------------------------------------


grid.newpage()
p2 %>% print(vp = viewport(width = .6, height = .6, angle = 45))

 
grid.newpage()
p %>% print(vp = viewport(width = .6, height = .6, angle = 45))



grid.newpage()
p_lines %>% print(vp = viewport(width = .6, height = .6, angle = 45))

svglite::svglite(file = "plots/2-35-simpsons.svg",
                 height = 15,
                 width = 15)
grid.newpage()
p_lines %>% print(vp = viewport(width = .6, height = .6, angle = 45))
dev.off()

