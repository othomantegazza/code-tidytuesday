# to get english weekdays
Sys.setlocale("LC_TIME", "en_US.UTF8")

library(tidyverse)
library(lubridate)
library(ggridges)

# load data ---------------------------------------------------------------

dat_path <- "data/27-birth.Rdata"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(paste0("https://raw.githubusercontent.com/",
                    "rfordatascience/tidytuesday/master/data/",
                    "2018-10-02/us_births_2000-2014.csv")) %>%
    # date is more useful in one single line
    # stored as date
    mutate(date = paste(year, month, date_of_month,
                        sep = "-"),
           date = date %>% lubridate::ymd())  %>%
    select(date, births)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


# plot --------------------------------------------------------------------

png(filename = "plots/27-births.png",
    height = 1500, width = 2400,
    res = 300)
dat %>%
  ggplot(aes(x = births,
             y = date %>% wday(label = TRUE) %>% fct_rev(),
             fill = ..x..)) +
  geom_density_ridges_gradient(alpha = .4, 
                               scale = 2.5, 
                               rel_min_height = .01) +
  scale_fill_viridis_c(option = "D") +
  theme_ridges() +
  theme(plot.caption = element_text(colour = "grey50")) +
  labs(x = "Births per day",
       y = "",
       title = "Births per Weekday",
       subtitle = "Measured in the US | 2000-2014",
       fill = "Births \n per day",
       caption = "Data: ssa.gov | Source: Fivethirtyeight | Plot by @othomn")
dev.off()  
