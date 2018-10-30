library(tidyverse)
library(magrittr)
library(cranlogs)


# Get data ----------------------------------------------------------------

# dat <-
#   installed.packages() %>%
#   as_tibble() %$%
#   Package %>%
#   cran_downloads(when = "last-month")
# 
# 
# save(dat, file = "data/31-cran-downloads.Rdata")

load("data/31-cran-downloads.Rdata")

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
                           to = c(0,1))) %T>%
  View()

dat_scaled %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile() +
  scale_fill_viridis_c()

dat %>%
  filter(package == "dplyr")
