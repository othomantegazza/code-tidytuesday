library(tidyverse)
library(ggridges)

# Download data -----------------------------------------------------------
# some rows are lost because external_author_id is parsed badly

if(!file.exists("data/20-rtrolls.Rdata")) {
  dat <- set_names(x = paste("https://github.com/fivethirtyeight/russian-troll-tweets/blob/master/IRAhandle_tweets_",
                             1:9,
                             ".csv?raw=true",
                             sep = ""), 
                   nm = as.character(1:9)) %>%
    map(read_csv) %>%
    purrr::reduce(bind_rows)
  save(dat, file = "data/20-rtrolls.Rdata")
} else {
  load("data/20-rtrolls.Rdata")
}

# dat <- read_csv(file = "data/IRAhandle_tweets_1.csv")


# Explore data ------------------------------------------------------------

dat %>% map(~length(unique(.)))

unique(dat$region)
table(dat$author)
table(dat$language)
hist(dat$followers)
table(dat$account_type)

dat %>%
  filter(account_type == "Right") %>%
  # View() %>%
  # group_by(author) %>%
  # tally() %>%
  # arrange(desc(n))
  filter(author == "AMELIEBALDWIN") %>%
  pull(content)
 

dat %>% 
  filter(followers > 20000) %>%
  pull(author) %>% unique()

dat_max <- dat %>%
  group_by(author, account_type) %>%
  summarise(max_followers = max(followers))

ggplot(dat_max,
       aes(x = reorder(account_type,
                       max_followers,
                       na.rm = TRUE),
           y = max_followers)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()

pdf(file = "plots/20-rtrolls.pdf",
    width = 5, height = 20)
ggplot(dat_max %>% 
         mutate(max_followers = max_followers + 1),
       aes(x = max_followers,
           y = account_type)) +
  ggridges::geom_density_ridges(alpha = .5) +
  scale_x_log10() +
  # facet_grid(account_type ~ ., scales = "free_y") +
  theme_bw()
dev.off()


ggplot(dat %>%
         filter(account_type %in% c("Russian", "Right", "left")) %>%
         # mutate(followers = log10(followers)) %>%
         filter(followers >= 1),
       aes(x = followers,
           y = account_type)) +
       # )) +
  ggridges::geom_density_ridges(alpha = .5) +
  # geom_histogram() +
  scale_x_log10() +
  # facet_grid(account_type ~ ., scales = "free_y") +
  ggridges::theme_ridges()

ggplot(dat_max %>%
         filter(account_type %in% c("Russian", "Right", "left")) %>%
         # mutate(followers = log10(followers)) %>%
         filter(max_followers >= 1),
       aes(x = max_followers,
           y = account_type)) +
  # )) +
  ggridges::geom_density_ridges(alpha = .5) +
  # geom_histogram() +
  scale_x_log10() +
  # facet_grid(account_type ~ ., scales = "free_y") +
  ggridges::theme_ridges()

ggplot(dat %>%
         filter(account_type %in% c("Russian", "Right", "left")) %>%
         # mutate(followers = log10(followers)) %>%
         filter(followers >= 1),
       aes(y = followers,
           x = account_type)) +
  # )) +
  geom_violin(alpha = .5) +
  # geom_histogram() +
  scale_y_log10() +
  # facet_grid(account_type ~ ., scales = "free_y") +
  theme_bw()

dat %>% pull(account_type) %>% table()

dat %>% filter(account_type == "Russian") %>%
  pull(content) 
