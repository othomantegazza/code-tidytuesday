Sys.setlocale("LC_TIME", "en_US.UTF8")
library(tidyverse)
library(lubridate)

# Read files --------------------------------------------------------------

local_data <- "data/hurrican-cover.Rdata"


if(!file.exists(local_data)) {
  paths <- c(google_trends = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_google_trends.csv",
             written_media = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_hurricanes.csv",
             states = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_states.csv",
             top_online_news = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_top_online_news.csv",
             trump = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_trump.csv",
             tv_media = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_tv_hurricanes.csv")
  
  dat <- paths %>%
    map(read_csv)
  
  # the first file complains
  # 
  # Parsed with column specification:
  #   cols(
  #     `Category: All categories` = col_character()
  #   )
  # Warning: 38 parsing failures.
  #
  # read manually
  
  dat$google_trends <- read_csv(paths["google_trends"], skip = 2)
  
  save(dat, file = "data/hurrican-cover.Rdata")
} else {
  load(local_data)
}


# Small tidyings to specific files------------------------------------------------

dat$written_media <- dat$written_media %>% mutate(Date = mdy(Date))

dat$states <- dat$states %>% mutate(Date = mdy(Date))

dat$tv_media <- dat$tv_media %>% 
  mutate(Date = mdy(Date)) %>%
  rename_at(.vars = vars(Harvey:Jose), .funs = ~paste0(., "_tv"))

dat$top_online_news <- NULL

dat$google_trends <- dat$google_trends %>% 
  rename(Date = "Day") 



# Tidy and merge into one dataset -----------------------------------------

# one observation per line
dat <- dat %>%
  map(~gather(data = .,
              key = "hurricane_state",
              value = "value",
              -Date))

# include data source in the dataset
# and merge
dat <- names(dat) %>%
  map(~mutate(dat[[.]], source = .)) %>%
  reduce(bind_rows)


# Plot reaction -----------------------------------------------------------

# last wrangling
dat <- dat %>%
  # Simple names for hurricanes
  mutate(hurricane = case_when(grepl("Harvey", hurricane_state) ~"Harvey",
                               grepl("Irma", hurricane_state) ~ "Irma",
                               grepl("Jose", hurricane_state) ~ "Jose",
                               grepl("Maria", hurricane_state) ~ "Maria",
                               TRUE ~ NA_character_)) %>%
  # descriptive name for data sources
  mutate(source = case_when(source == "google_trends" ~ "Google Search Trend",
                            source == "tv_media" ~ "Percent of Sentences in TV News",
                            source == "written_media" ~ "N. of Sentences in Online News")) %>%
  filter(complete.cases(.),
         value > 0)

# and plot
jpeg(filename = "plots/hurricane_media.jpg",
     width = 8, 
     height = 5,
     units = "in",
     res = 200)
ggplot(dat,
       aes(x = Date, y = value)) +
  geom_col(fill = "#007399",
           alpha = .5) + #0099cc") +
  facet_grid(source ~ hurricane,
             scales = "free",
             space = "free_x",
             labeller = label_wrap_gen(width = 10,
                                       multi_line = T)) +
  ggtitle("Hurricane Mentions in Media") +
  ylab("Recurrence in Media") +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 1),
        strip.text.y = element_text(angle = 0))

dev.off()