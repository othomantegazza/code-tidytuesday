library(tidyverse)
library(magrittr)
library(cranlogs)
library(lubridate)
library(ggridges)



# Set time span of one year to now ----------------------------------------

to <- today()

from <- to %m-% months(12)

# Get data ----------------------------------------------------------------

# dat <-
#   installed.packages() %>%
#   as_tibble() %$%
#   Package %>%
#   # cran_downloads(when = "last-month")
#   cran_downloads(from = from,
#                  to = to)
# 
# 
# save(dat, file = "data/31-cran-downloads.Rdata")

load("data/31-cran-downloads.Rdata")



# Tidy --------------------------------------------------------------------

# kludge, remove a bigg spike in tidyverse downloads
dat <- 
  dat %>%
  filter(date != max(date),
         count != max(count))

# remove packages with 0 counts
no_counts <- 
  dat %>%
  group_by(package) %>%
  summarise(counts = sum(count)) %>%
  filter(counts == 0) %$%
  package

dat <- 
  dat %>%
  filter(! package %in% no_counts) %>%
  as_tibble()


# Poisson model  ----------------------------------------------------------

# as in: TREND DETECTION IN SOCIAL DATA by Scott Hendickson

dat %>% 
  filter(package == "acepack") %>%
  ggplot(aes(x = date,
             y = count)) +
  geom_point() +
  geom_line() +
  theme_bw()

# set roll median function
roll_median_7 <- tibbletime::rollify(~median(., na.rm = T), 30)

# poisson model over same day one week ago
# to much variance
# model p-value on longer time span?
dat_test <- dat %>% 
  # filter(package == "acepack") %>%
  group_by(package) %>%
  arrange(date) %>% 
  mutate(count = count %>% na_if(0),
         prev_count = lag(count, 7)) %>% 
  mutate(p = ppois(q = count,
                   lambda = prev_count,
                   lower.tail = FALSE) %>%
           na_if(-Inf),
         log_p = -log(p)) %>% 
  # roll mean
  ungroup() %>% 
  group_by(package) %>%
  arrange(date) %>% 
  mutate(med_log = log_p %>%
           roll_median_7()) %>% 
  ungroup()
  
trending_packs <- 
  dat_test %>%
  filter(date == max(date)) %>%
  arrange(med_log) %>%
  dplyr::top_n(n = 5) %>%
  pull(package)

dat_test %>% 
  # filter(package == "ggraph") %>% #View()
  filter(package %in% trending_packs) %>%
  gather(key = "measure",
         value = "value",
         count, log_p, med_log) %>%
  ggplot(aes(x = date,
             y = value)) +
  geom_point() +
  geom_line() +
  facet_grid(measure ~ package, scales = "free_y") +
  theme_bw()

# Two tops ----------------------------------------------------------------

# estimate on last 14 days

top_pack <- 
  dat %>% 
  filter(date >= max(date) - days(14)) %>%
  group_by(package) %>% 
  summarise(count = sum(count)) %>% 
  top_n(2) %>% 
  pull(package)

dat %>%
  filter(package %in% top_pack) %>% 
  rename(downloads = "count") %>% 
  ggplot(aes(x = date,
             y = downloads,
             group = package)) +
  geom_point() +
  geom_line() +
  facet_grid(package ~ .) +
  theme_bw()

