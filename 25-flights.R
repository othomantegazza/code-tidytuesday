library(tidyverse)


# Read Data ---------------------------------------------------------------

dat_path <- "data/25-flights.Rdata"


if(!file.exists(dat_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/master/data/",
                         "2018-09-18/us-airports.csv")) %>%
    select(-X1)
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# Analyze data ------------------------------------------------------------

# Many missing data from 2014 onward ?

dat %>%
  split(.$year) %>%
  # map(~sum(is.na(.$passengers)))
  map(dim)

tst <- dat %>%
  select(loc_id, hub_type,
         year, passengers, state) %>%
  spread(key = year, value = passengers) %>%
  filter(complete.cases(.),
         hub_type != "Nonhub")


# plot --------------------------------------------------------------------

tst %>%
  ggplot(aes(x = `2012`,
             y =`2017`/`2012`)) +
  geom_point() +
  geom_point(data = tst %>% filter(state == "OH"),
             colour = "blue") +
  # geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()
