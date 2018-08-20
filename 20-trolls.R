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


# Get top types ------------------------------------------------------------

top_accounts <- dat %>%
  group_by(account_type) %>%
  tally() %>%
  arrange(desc(n)) %>%
  top_n(6) %>%
  pull(account_type)


# plot top accounts -------------------------------------------------------

jpeg(filename = "plots/rtrolls.jpg",
     width = 7, 
     height = 5,
     units = "in",
     res = 200)
ggplot(dat %>%
         filter(account_type %in% top_accounts,
                post_type != "RETWEET") %>%
         # mutate(followers = log10(followers)) %>%
         filter(followers >= 1),
       aes(x = followers,
           y = account_type)) +
       # )) +
  ggridges::geom_density_ridges(alpha = .5, fill = "lightblue") +
  # geom_histogram() +
  scale_x_log10() +
  # facet_grid(account_type ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Followers of Different IRA Account Types",
          subtitle = "Excluding retweets") +
  xlab("Number of Followers at Time of the Tweet (log scale)") +
  ylab("Specific Account Type (by Linvill and Warren)")
dev.off()
