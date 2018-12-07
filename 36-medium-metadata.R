library(tidyverse)


# get data ----------------------------------------------------------------

dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/",
                  "data/2018-12-04/medium_datasci.csv?raw=true")

dat_path <- "data/36-medium-metadata.Rdata"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(dat_url) %>% 
    select(-x1)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


# explore -----------------------------------------------------------------

dat$title %>% unique() %>% length()

dat %>% 
  ggplot(aes(x = reading_time,
             y = claps)) +
  geom_point(alpha = .1) +
  scale_x_log10() +
  scale_y_log10()

dat %>% filter(claps > 10000) %>% View()
dat %>% filter(claps > 10000) %>% 
  select(contains("tag")) %>% 
  mutate(sums = pmap_dbl(.,sum)) %>% 
  # pull(sums) %>% 
  # range()
  filter(sums > 1) %>% View

dat %>% top_n(50, wt = claps) %>% pull(author) %>% table()

dat %>% 
  mutate_at(vars(matches("tag")),
            .funs = funs(.*claps)) %>% 
  summarise_at(.vars = vars(matches("tag")), sum)


# plot --------------------------------------------------------------------

clap <- dat %>% 
  mutate_at(vars(matches("tag")),
            .funs = funs(.*claps)) %>% 
  summarise_at(.vars = vars(matches("tag")),
               sum) %>% 
  mutate(measure = "claps", 
         tot = pmap_dbl(., sum)) %>% 
  mutate_at(vars(matches("tag")),
            .funs = funs(./tot))


posts <- dat %>% 
  summarise_at(.vars = vars(matches("tag")), 
               sum) %>% 
  mutate(measure = "posts",
         tot = pmap_dbl(., sum)) %>% 
  mutate_at(vars(matches("tag")),
            .funs = funs(./tot))

to_plot <- 
  bind_rows(clap, posts) %>% 
  select(-tot) %>% 
  gather(tag_ai:tag_machine_learning,
         key = "tag",
         value = "percent")

p <- 
  to_plot %>% 
  arrange(percent) %>% 
  mutate(tag = tag %>% 
           str_remove("tag_") %>% 
           str_replace("_", "\n") %>% 
           as_factor()) %>% 
  ggplot(aes(x = measure,
              y = percent,
              fill = tag)) +
  geom_bar(stat = "identity",
           colour = "white", size = .8) +
  coord_flip(expand = F) +
  theme_minimal() +
  labs(title = "Deep and Machine Learning Posts Get Clapped More",
       subtitle = "Share of posts and claps for a set of 78k posts on Medium with data related tags",
       x = "",
       caption = "Source: medium.com, kaggle.com, M. Hendirckson, K. Misra | Plot by @othomn") +
  scale_fill_viridis_d(
    option = "B",
    guide = guide_legend(title.vjust = .2,
                         label.position = "top",
                         keyheight = unit(4, units = "mm"),
                         keywidth=unit(14, units = "mm"), 
                         nrow = 1,
                         reverse = TRUE)) +
  theme(text = element_text(colour = "grey20",
                            family = "sans"),
        legend.position = "top",
        plot.margin = margin(t = 10, l = 3.4,
                             b = 6, r = 5, unit = "mm"))

png(filename = "plots/36-medium-metadata.png",
    height = 1100, width = 2200,
    res = 300)
p %>% print()
dev.off()   

