library(tidyverse)
library(ggrepel)

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

cutoff <- 250
cut_lm <- 130
fit <- lm(rush_yds ~ rush_att, data = dat)
dat <- dat %>%
  mutate(pred_rush_yds = predict(fit, newdata = .)) %>%
  mutate(pred_rush_diff = rush_yds - pred_rush_yds)

# jpeg(filename = "plots/22-nfl.jpg",
#      width = 1000,
#      height = 1000)
png(filename = "plots/22-nfl.png",
    height = 1700, width = 2500,
    res = 300)
dat %>%
  # group_by(name,
  #          game_year) %>%
  # summarize(rush_att = sum(rush_att),
  #           rush_yds = sum(rush_yds)) %>%
  ggplot(aes(x = rush_att,
             y = rush_yds)) +
  geom_hex() +
  geom_point(data = . %>% 
               # filter(rush_yds > cutoff),
               filter(pred_rush_diff > cut_lm),
              aes(x = rush_att,
                  y = rush_yds), 
              colour = "red",
             size = 2,
             alpha = .5) +
  geom_smooth(method = "lm", colour = "darkred") +
  stat_smooth(aes(x = rush_att,
                  y = rush_yds + cut_lm),
              geom = "line",
              method = lm, colour = "red",
              alpha = .3, linetype = "dashed") +
  ggrepel::geom_text_repel(data = . %>%
                           # filter(rush_yds > cutoff),
                            filter(pred_rush_diff > cut_lm), 
                            aes(x = rush_att,
                                y = rush_yds,
                                label = paste(name, game_year,
                                              sep = "\n")),
                           colour = "red",
                           size = 3) +
  scale_fill_gradient(low = "lightgrey", high = "black",
                      trans = "log",
                      breaks = c(2, 10, 50, 250, 1250, 6000)) +
  # geom_point() +
  theme_bw() +
  labs(title = "Results of Rush Attempts in Yards",
       subtitle = "Recorded in NFL from 2000 to 2017",
       caption = "Source: www.pro-football-reference.com, plot by @othomn",
       x = "Rush Attempts",
       y = "Rush Yards")
dev.off()

