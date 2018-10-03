Sys.setlocale("LC_TIME", "en_US.UTF8")
library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(ggridges)

# load data ---------------------------------------------------------------

dat_path <- "data/27-birth.Rdata"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(paste0("https://raw.githubusercontent.com/",
                    "rfordatascience/tidytuesday/master/data/",
                    "2018-10-02/us_births_2000-2014.csv")) %>%
    mutate(date = paste(year, month, date_of_month,
                        sep = "-"),
           date = date %>% lubridate::ymd())  %>%
    select(date, births)
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


# explore -----------------------------------------------------------------

dat %>%
  ggplot(aes(x = date %>% wday(label = TRUE),
             y = births)) +
  geom_violin(draw_quantiles = .5) +
  # geom_boxplot(colour = "grey30", ) +
  geom_quasirandom(alpha = .1) +
  theme_bw() 

dat %>%
  ggplot(aes(x = births,
             y = date %>% wday(label = TRUE) %>% fct_rev(),
             fill = ..x..)) +
  geom_density_ridges_gradient(alpha = .4, 
                               scale = 2.5, 
                               rel_min_height = .01) +
  scale_fill_viridis_c(option = "D") +
  theme_ridges() +
  labs(x = "Births per day",
       y = "",
       title = "Birth per Weekday",
       subtitle = "Measured in the US | 2000-2014",
       fill = "Births \n per day")
  
