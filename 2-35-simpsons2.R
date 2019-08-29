library(tidyverse)
library(grid)
library(showtext)


font_add_google("Major Mono Display",
                family = "titles")

"The Girl Next Door"
"Text Me One" 
"Yrsa"
"Dancing Script" %>% font_add_google(family = "labels")

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


# tidy --------------------------------------------------------------------


simpsons2 <- 
  simpsons %>% 
  mutate(season = as.numeric(season)) %>% 
  separate(col = number, into = c("episode_count", "season_count"), sep = "–") %>% 
  mutate(episode_count = as.numeric(episode_count)) %>% 
  drop_na(season)



# plot --------------------------------------------------------------------

stars_in <- 
  simpsons2 %>% 
  group_by(guest_star) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(guest_star)

violet <- "#6D1EC9"
orange <- "#FF6C0D"
blue <- "#263A89"

colors <- rep(c(violet, orange, blue), length.out = max(simpsons2$season, na.rm = T))

p <- 
  simpsons2 %>% 
  filter(guest_star %in% stars_in) %>%
  ggplot(aes(x = guest_star %>% as_factor() %>% fct_rev() %>% as.numeric())) +
  # lines for guest stars
  geom_line(aes(y = episode_count,
                group = guest_star),
            size = .2,
            alpha = 1,
            colour = "grey30") +
  # lines for episodes
  geom_line(aes(y = episode_count,
                group = episode_count,
                colour = as.character(season)),
            size = .2,
            linetype = 1,
            alpha = 1) +
  # point each cross episode and guest star
  geom_point(aes(y = episode_count,
                 colour = as.character(season)),
             size = .15,
             # colour = "grey30",
             alpha = 1) +
  # guest star name
  geom_text(data = . %>% group_by(guest_star) %>% filter(episode_count == min(episode_count)),
            aes(y = episode_count,
                label = guest_star),
            size = 1.8,
            hjust = 1.1,
            vjust = .3,
            angle = 45,
            family = "labels",
            colour = "grey20") +
  # season mark
  geom_segment(data = . %>% group_by(season) %>%
              summarize(max_s = max(episode_count),
                        min_s = min(episode_count)),
              aes(x = 136,
                  xend = 136,
                  y = min_s,
                  yend = max_s,
                  colour = season %>% as.character()),
              size = .6,
              alpha = .7) +
  # season label
  geom_text(data = . %>% group_by(season) %>%
              summarize(y = mean(episode_count)),
            aes(x = 137,
                label = paste("Season", season),
                y = y,
                colour = season %>% as.character()),
            hjust = 0,
            vjust = .3,
            size = 3,
            alpha = .7,
            family = "labels") +
  scale_colour_manual(values = colors, guide = FALSE) +
  theme_void()

p


# save in grid ------------------------------------------------------------

height <- 20
width <- 14

svglite::svglite(file = "plots/2-35-simpsons3.svg",
                 height = height,
                 width = width)
grid.newpage()
# grid.rect(gp = gpar(fill = "#F1F3F4"))
showtext_begin()
p %>% print(vp = viewport(x = .3, width = .94, height = .94*(width/height), angle = 315))
str_wrap("The Simposons, by Matt Groening, have been running for more the 30 seasons
         and had hundreds of guest stars, acting as themeselves or someone else.",
         width = 14) %>% 
  grid.text(hjust = 0, rot = 45,
            x = .63, y = .74, 
            gp = gpar(lineheight = 1,
                      fontsize = 14,
                      fontfamily = "titles"))
showtext_end()
dev.off()
 
