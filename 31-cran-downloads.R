library(tidyverse)
library(cranlogs)
library(lubridate)
library(tibbletime)
library(glue)


# Set time span of one year to now ----------------------------------------

to <- today() - days(1)

from <- to %m-% months(12)

# Get data ----------------------------------------------------------------

data_path <- "data/31-cran-downloads.Rdata"

if(!file.exists(data_path)) {
  dat <-
    installed.packages() %>%
    as_tibble() %$%
    Package %>%
    cran_downloads(from = from,
                   to = to)
  
  save(dat, file = data_path)
} else {
  load(data_path)
}

# Tidy --------------------------------------------------------------------

# Remove a big spike in tidyverse downloads
dat <- 
  dat %>%
  mutate(count = case_when(package == "tidyverse" & count == 835133 ~ NA_real_,
                           TRUE ~ count))

# Remove packages with 0 counts
# They come from other repositories than cran

no_counts <- 
  dat %>%
  group_by(package) %>%
  summarise(counts = sum(count)) %>%
  filter(counts == 0) %>%
  pull(package)

dat <- 
  dat %>%
  filter(! package %in% no_counts) %>%
  as_tibble()


# Poisson model  ----------------------------------------------------------

# inspired by: TREND DETECTION IN SOCIAL DATA by Scott Hendickson

# On how many days should I roll the median ?
# this parameter controls the responsiveness of the algorithm!!
n_med <- 30
n_mean <- 14
# set roll median function
roll_median <- tibbletime::rollify(~median(., na.rm = T), n_med)
roll_mean <- tibbletime::rollify(~mean(., na.rm = T), n_mean)

# They work with poisson confidence intervals,
# Why? because they respond better then pvalues when
# measurement are far away from expected value?
get_upper_bound <- function(n) {
  if(is.na(n)) {
    NA_real_
  } else {
    poisson.test(n, conf.level = 0.99)$conf.int[2]
  }
}

# Get an estimate of the distance
dat_test <- dat %>% 
  group_by(package) %>%
  arrange(date) %>% 
  mutate(count = count %>% na_if(0),
         prev_count = lag(count, 7)) %>% 
  mutate(p = ppois(q = count,
                   lambda = prev_count,
                   lower.tail = FALSE) %>%
           na_if(-Inf),
         log_p = -log(p)) %>% 
  mutate(up_bound = map(prev_count, get_upper_bound) %>% purrr::flatten_dbl(),
         conf = up_bound - prev_count,
         dist = (count - prev_count) / conf) %>%
  ungroup() %>% 
  group_by(package) %>%
  arrange(date) %>% 
  mutate(med_log = log_p %>%
           roll_median(),
         mean_dist = dist %>%
           roll_mean()) %>% 
  ungroup()
  
trending_packs <- 
  dat_test %>%
  filter(date == max(date)) %>%
  arrange(mean_dist) %>%
  dplyr::top_n(n = 5) %>%
  pull(package)


# Plot results ------------------------------------------------------------

plot_trend <- function(package = "clipr",
                       n = 1)
{
  dat_test %>% 
    filter(package == !!package) %>%
    gather(key = "measure",
           value = "value",
           count, med_log, mean_dist) %>% 
    ggplot(aes(x = date,
               y = value)) +
    geom_density(stat = "identity",
                 colour = NA,
                 fill = "red") +
    facet_grid(measure ~ ., 
               scales = "free_y") +
    theme_minimal()+
    labs(title = glue("#{n} {package}"))
}

to_plot <- tibble(n = 5:1,
                  package = trending_packs) %>%
  arrange(n) %>% 
  pmap(plot_trend) %>%
  map(print)

