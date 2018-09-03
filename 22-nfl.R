library(tidyverse)

# Get data ----------------------------------------------------------------


nfl_path <- "data/22-nfl.Rdata"
  
if(!file.exists(nfl_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/",
                         "master/data/2018-08-28/nfl_2010-2017.csv"))
  save(dat, file = nfl_path)
} else {
  load(nfl_path)
}


# Explore data ------------------------------------------------------------

dat %>% 
  group_by(game_year) %>%
  summarise(tot_rush = sum(rush_yds, na.rm = T),
            tot_pass = sum(pass_yds, na.rm = T))

jpeg(filename = "plots/22-nfl.jpg",
     width = 1000,
     height = 1000)
dat %>%
  ggplot(aes(x = rush_att,
             y = rush_yds)) +
  geom_hex() +
  geom_point(data = dat %>% filter(rush_yds > 250),
              aes(x = rush_att,
                  y = rush_yds), 
              colour = "red",
             size = 2,
             alpha = .5) +
  ggrepel::geom_text_repel(data = dat %>% filter(rush_yds > 250),
                            aes(x = rush_att,
                                y = rush_yds,
                                label = paste(name, game_year)),
                           colour = "red") +
  scale_fill_gradient(trans = "log",
                      breaks = c(2, 10, 50, 250, 1250, 6000)) +
  # geom_point() +
  theme_bw()
dev.off()

dat %>%
  ggplot(aes(x = rush_att  %>% as.character() %>% as_factor(),
             y = rush_yds)) +
  geom_boxplot()
  geom_point()
