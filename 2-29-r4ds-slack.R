library(tidyverse)
library(tibbletime)
library(grid)


# colors
purple <- "#AA2255"
purple2 <- "#BB2255"
bg_col <- "#EAEA9F"
blue <- "#263A89"

# load data ---------------------------------------------------------------


data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

data_path <- "data/2-29-r4ds-slack.Rdata"


if(!file.exists(data_path)) {
  r4ds <- 
    data_url %>% 
    read_csv() %>% 
    select(-name)
  
  save(r4ds, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

# no na
r4ds %>% map(~is.na(.) %>% sum())

# correlations
prs <- r4ds %>% GGally::ggpairs()

png(filename = "plots/2-29-r4ds-slack.png",
    width = 3000,
    height = 3000,
    res = 400)
prs
dev.off()

# what is name?
# r4ds %>% pull(name) %>% range()
# edited name, can be removed


# all timelines
r4ds %>% 
  gather(total_membership:messages_posted,
         key = "key", value = "value") %>% 
  ggplot(aes(x = date,
             y = value)) +
  geom_line() +
  facet_grid(key ~ ., scales = "free_y") +
  theme(strip.text.y = element_text(angle = 0))


# Active members ----------------------------------------------------------

# new years resolution?

roll_weekmean <- rollify(mean, window = 7)

roll_monthmean <- rollify(mean, window = 30)

text_height <- 240

p <- 
  r4ds %>% 
  mutate(weekmean = roll_weekmean(daily_active_members) %>% 
           # recenter rolled mean
           .[c(4:n(), 1:3)]) %>% 
  ggplot(aes(x = date,
             y = daily_active_members)) +
  geom_density(stat = "identity", colour = NA,
               fill = "white") +
  geom_line(colour = "#E4E484") + #"#99D0E3") + # "#27A6D3") +
  geom_line(aes(y = weekmean), colour = purple) +
  annotate(geom = "text",
           label = "New Year's Resolutions? ;)",
           y = text_height,
           x = as.Date("2018-07-15"), 
           family = "courier",
           colour = blue) +
  annotate(geom = "curve",
           x = as.Date("2018-04-01"),
           y = text_height,
           xend = as.Date("2018-01-15"),
           yend = 160,
           curvature = .25,
           arrow = arrow(length = unit(1.2, "mm"), type = "closed"),
           size = .1,
           colour = blue) +
  annotate(geom = "curve",
           x = as.Date("2018-10-27"),
           y = text_height,
           xend = as.Date("2019-01-20"),
           yend = 155,
           curvature = -.26,
           arrow = arrow(length = unit(1.2, "mm"), type = "closed"),
           size = .1,
           colour = blue) +
  annotate(geom = "text",
           label = str_wrap("Mean of 7 days window.", width = 10),
           y = 130,
           x = as.Date("2019-07-22"), 
           family = "courier",
           colour = purple,
           size = 3,
           hjust = 0,
           lineheight = 1) +
  annotate(geom = "curve",
           x = as.Date("2019-08-10"),
           y = 104,
           xend = as.Date("2019-07-10"),
           yend = 70,
           curvature = -.45,
           arrow = arrow(length = unit(1.2, "mm"), type = "closed"),
           size = .1,
           colour = purple) +
  labs(x = "",
       y = "Daily active members",
       title = "Activity of the R4DS Learning Community on Slack",
       caption = "Source: R4DS UseR Presentation | Plot by @othomn") +
  theme_minimal(base_family = "courier") +
  scale_x_date(limits = as.Date(c("2017-08-20", "2019-09-05")),
               expand = c(0,0)) +
  theme(panel.grid = element_blank(),
        plot.margin = margin(10, 20, 5, 30),
        axis.title = element_text(colour = "grey30"),
        plot.title = element_text(colour = blue,
                                  face = "bold",
                                  margin = margin(t = 10, b = 10)),
        plot.caption = element_text(colour = purple))


# save plot ---------------------------------------------------------------

png(filename = "plots/2-29-r4ds-slack.png",
    height = 1000,
    width = 3200,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
print(p, vp = viewport())
dev.off()
