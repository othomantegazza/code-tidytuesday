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
  simpsons2 %>% 
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


# lines vs lines ----------------------------------------------------------

stars_in <- 
  simpsons3 %>% 
  group_by(guest_star) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(guest_star)

p_lines2 <- simpsons3 %>% 
  filter(guest_star %in% stars_in) %>%
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev())) +
  geom_point(aes(y = y),
             size = .2,
             alpha = .4) +
  geom_line(aes(y = y,
                group = episode_count),
            size = .2,
            alpha = .7) +
  geom_line(aes(group = guest_star,
                y = y),
            size = .2,
            alpha = .7) +
  coord_flip() +
  guides(colour = FALSE,
         fill = FALSE) +
  theme_void()

p_lines2

svglite::svglite(file = "plots/2-35-simpsons.svg",
                 height = 15,
                 width = 15)
grid.newpage()
p_lines2 %>% print(vp = viewport(width = .6, height = .6, angle = 45))
dev.off()


# fix episode lines -------------------------------------------------------

simpsons4 <- 
  simpsons3 %>% 
  filter(guest_star %in% stars_in) %>% 
  mutate(guest_star = guest_star %>% as_factor()) 

episodes <- 
  simpsons4 %>% 
  pull(episode_count) %>% 
  unique()

g_stars <- 
  simpsons4 %>% 
  group_by(episode_count) %>% 
  summarise(ep_high = max(as.numeric(guest_star)),
            ep_low = min(as.numeric(guest_star)))

episode_lines <- 
  expand.grid(guest_star = simpsons4 %>% pull(guest_star) %>% unique(),
              episode_count = episodes) %>% 
  left_join(start_at) %>% 
  left_join(g_stars) %>% 
  left_join(simpsons4 %>% select(season, episode_count)) %>% 
  mutate(y = episode_count - (ep_min )) %>% 
  filter(y > 0) %>% 
  mutate(guest_star = factor(guest_star, levels = levels(simpsons4$guest_star))) %>% 
  filter(as.numeric(guest_star) <= ep_high,
         as.numeric(guest_star) >= ep_low) 

# episode_lines %>% 
#   filter(episode == 100) %>% 
#   View()

p_lines3 <- simpsons4 %>% 
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev())) +
  # point at episode guest star
  geom_point(aes(y = y),
             size = .2,
             alpha = .4) +
  # line for eacg guest star
  geom_line(aes(group = guest_star,
                y = y),
            size = .6,
            alpha = .4) +
  # episodes
  geom_line(data = episode_lines,
            aes(y = y,
                group = episode_count,
                colour = season),
            size = .6,
            alpha = .4) +
  coord_flip() +
  guides(colour = FALSE,
         fill = FALSE) +
  theme_void()

p_lines3

svglite::svglite(file = "plots/2-35-simpsons2.svg",
                 height = 20,
                 width = 20)
grid.newpage()
p_lines3 %>% print(vp = viewport(width = .6, height = .6, angle = 45))
dev.off()
