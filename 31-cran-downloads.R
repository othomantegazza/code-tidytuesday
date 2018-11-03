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

# temporary
dat <- 
  dat %>%
  filter(date != max(date),
         count != max(count))

# dat %>% View()

# Tidy --------------------------------------------------------------------

# packages with 0 counts?
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

# Plot --------------------------------------------------------------------

dat_scaled <- 
  dat %>% 
  group_by(package) %>%
  filter(count > 0) %>%
  mutate(scaled_count = count %>%
           log() %>%
           scales::rescale(from = range(.),
                           to = c(0,1)))

p <- dat_scaled %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()

p + scale_fill_viridis_c()

png("plots/31-cran-test.png",
    height = 2000,
    width = 700,
    res = 300)
p + 
  scale_fill_viridis_c(guide = FALSE) +
  theme_void()
dev.off()

dat_scaled %>% 
  ggplot(aes(x = date,
             y = count %>% log(),
             group = package)) +
  geom_line(alpha = .3) +
  theme_bw()


# Subset ------------------------------------------------------------------

set.seed(1)

pack_sub <- 
  dat_scaled$package %>% 
  unique() %>%
  sample(100)

p_subset <- 
  dat_scaled %>%
  filter(package %in% pack_sub) %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()
  
p_subset + scale_fill_viridis_c()
  

png("plots/31-cran-test-subset.png",
    height = 1000,
    width = 700,
    res = 300)
p_subset + 
  scale_fill_viridis_c(guide = FALSE) +
  theme_void()
dev.off()

# Poisson model  ----------------------------------------------------------

# as in: TREND DETECTION IN SOCIAL DATA by Scott Hendickson

dat %>% 
  filter(package == "acepack") %>%
  ggplot(aes(x = date,
             y = count)) +
  geom_point() +
  geom_line() +
  theme_bw()


# poisson model over same day one week ago
# to much variance
# model p-value on longer time span?
tst <- dat %>% 
  filter(package == "acepack") %>%
  arrange(date) %>% 
  mutate(count = count %>% na_if(0),
         prev_count = count[c(rep(NA, 7),
                              1:(n() - 7))]) %>% 
  mutate(p = ppois(q = count,
                   lambda = prev_count,
                   lower.tail = FALSE) %>%
           na_if(-Inf),
         log_p = -log(p))
  
tst %>% 
  filter(package == "acepack") %>%
  gather(key = "measure",
         value = "value",
         count, log_p) %>%
  ggplot(aes(x = date,
             y = value)) +
  geom_point() +
  geom_line() +
  facet_grid(measure ~ ., scales = "free_y") +
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


# Poisson on weeks --------------------------------------------------------

dat %>%
  mutate(week = )
