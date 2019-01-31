library(tidyverse)
library(lubridate)
library(rlang)
library(gtable)
library(gridExtra)
library(grid)
library(scico)

# Get Data ----------------------------------------------------------------


dat_path <- "data/2-05-milkcow-facts.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "/2019/2019-01-29/milkcow_facts.csv")


if(!file.exists(dat_path)) {
  dat_milkcow <- 
    read_csv(dat_url)
  
  save(dat_milkcow, file = dat_path)
  
} else {
  load(dat_path)
}


# Explore -----------------------------------------------------------------

library(GGally)

ggpairs(dat_milkcow)


# Clean chese -------------------------------------------------------------

dat_path <- "data/2-05-clean-cheese.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-29/clean_cheese.csv")

if(!file.exists(dat_path)) {
  dat_cheese <- 
    read_csv(dat_url) %>% 
    rename_all(tolower)
  
  save(dat_cheese, file = dat_path)
  
} else {
  load(dat_path)
}


# explore -----------------------------------------------------------------

# dat %>% 
#   gather(Cheddar:Blue,
#          key = "chees_type",  value = "lbs") %>% 
#   ggplot(aes(x = Year, 
#              y = lbs,
#              fill = chees_type)) +
#   geom_bar(stat = "identity")


# Milk products -------------------------------------------------------------

dat_path <- "data/2-05-milk-product-facts.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-29/milk_products_facts.csv")

if(!file.exists(dat_path)) {
  dat_milkprods <- 
    read_csv(dat_url)
  
  save(dat_milkprods, file = dat_path)
  
} else {
  load(dat_path)
}


# explore -----------------------------------------------------------------

# dat %>% 
#   gather(fluid_milk:fluid_yogurt,
#          key = "type",  value = "lbs") %>% 
#   ggplot(aes(x = year, 
#              y = lbs,
#              fill = type)) +
#   geom_bar(stat = "identity")


# explore all -------------------------------------------------------------

dat <- 
  dat_cheese %>%
  full_join(dat_milkprods, by = "year") %>%
  gather(cheddar:dry_whey, key = "type", value = "lbs")
  

dat_dry <- 
  dat %>%
  filter(str_detect(type, "dry")) %>% 
  group_by(year) %>% 
  summarise(lbs = sum(lbs)) %>% 
  mutate(type = "dry")


dat_evap <- 
  dat %>%
  filter(str_detect(type, "evap")) %>% 
  group_by(year) %>% 
  summarise(lbs = sum(lbs)) %>% 
  mutate(type = "evap")


dat_short <- 
  dat %>%
  filter(!str_detect(type, "dry"),
         !str_detect(type, "evap")) %>% 
  bind_rows(dat_dry) %>% 
  bind_rows(dat_evap)
  

dat %>%  
  ggplot(aes(x = year,
             y = lbs)) +
  geom_line() +
  facet_wrap("type", scales = "free")

dat_short %>%  
  ggplot(aes(x = year,
             y = lbs)) +
  geom_line() +
  facet_wrap("type", scales = "free")


up <- c("butter", "fluid_yogurt", "italian other", 
        "mozzarella", "total american cheese")
down <- c("fluid_milk", "evap")

dat_short %>% 
  filter(type %in% down) %>% 
  ggplot(aes(x = year,
             y = lbs,
             fill = type)) +
  geom_bar(stat = "identity", width = 1, alpha = .5)

dat_short %>% 
  filter(type %in% c(up, down)) %>% 
  ggplot(aes(x = year,
             y = lbs,
             fill = type)) +
  geom_bar(stat = "identity", width = 1, alpha = .5)

library(tibbletime)

roll_percent <- rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)

dat_perc <- 
  dat_milkprods %>% 
  select(year, fluid_milk) %>% 
  mutate(percent = roll_percent(fluid_milk)) %>% 
  filter(complete.cases(.)) 

dens <-  
  dat_perc$percent %>% 
  density(n = 2^12) %>% 
  {tibble(x = .$x,
          y = .$y)}

dens %>% 
  ggplot(aes(x = x,
             y = y,
             xend = x)) +
  geom_segment(aes(colour = x),yend = 0) +
  geom_line(color = "grey30", size = 2) +
  # scale_color_viridis_c(option = "E")
  scico::scale_color_scico(palette = "roma") +
  theme_minimal()
