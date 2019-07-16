library(tidyverse)
library(tibbletime)
library(grid)

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


roll_weekmean <- rollify(mean, window = 7)

roll_monthmean <- rollify(mean, window = 30)

r4ds %>% 
  mutate(weekmean = roll_weekmean(daily_active_members),
         monthmean = roll_monthmean(daily_active_members)) %>% 
  ggplot(aes(x = date,
             y = daily_active_members)) +
  # geom_line() +
  geom_density(stat = "identity", colour = NA, fill = "grey80") +
  # map(seq(0, 200, by = 5), ~geom_hline(yintercept = ., colour = "white")) +
  # geom_ribbon(aes(ymin = weekmean - 3, ymax = weekmean + 3)) +
  geom_line(aes(y = weekmean)) +
  # geom_line(aes(y = monthmean), colour = "purple") +
  # stat_smooth() +
  theme_minimal()
