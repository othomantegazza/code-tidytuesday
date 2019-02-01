library(tidyverse)
library(scico)
library(tibbletime)

# Get Data milk cow -------------------------------------------------------


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


# Get data Clean chese -----------------------------------------------------

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

# Get data Milk products ---------------------------------------------------

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


# Percent density ---------------------------------------------------------

# roll over tibbles
roll_percent <- rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)


# With interesting patterns
up <- c("butter", #"fluid_yogurt", 
        "mozzarella", "total american cheese")
down <- "fluid_milk"

keep <- c(down, up)

# why are those two dataset separated?
dat <- 
  dat_cheese %>%
  full_join(dat_milkprods, by = "year") %>%
  gather(cheddar:dry_whey,
         key = "type", value = "lbs") %>% 
  filter(complete.cases(.)) %>% 
  filter(type %in% keep)

dat_perc <-  
  dat %>%
  group_by(type) %>%
  mutate(percent = roll_percent(lbs)) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) 

# how do I prepare a density dataset?
get_density <- function(type_of_cheese) 
{
  dat_perc %>% 
    filter(type == type_of_cheese) %>% 
    pull(percent) %>% 
    density(n = 2^12) %>% 
    {tibble(type = type_of_cheese,
            x = .$x,
            y = .$y)}
}


dens <- 
  dat_perc$type %>% 
  unique() %>% 
  map_df(get_density)

# center the divergent fill to 0
fill_lim <- dens$x %>% range() %>% abs() %>% max()

dens %>% 
  ggplot(aes(x = x,
             y = y,
             xend = x)) +
  geom_segment(aes(colour = x),yend = 0) +
  geom_line(color = "grey30", size = 1.5) +
  facet_wrap(facets = "type", 
             ncol = 1, scales = "free_y") +
  # scale_color_viridis_c(option = "E")
  scale_color_scico(palette = "roma",
                    direction = 1,
                    limits =  c(-fill_lim, fill_lim)) +
  theme_grey() 


dat_perc %>% 
  filter

dat_perc %>% 
  ggplot(aes(x = year,
             y = percent,
             xend = year)) +
  geom_line() +
  geom_segment(aes(colour = percent),
               yend = 0) +
  # geom_area(aes(fill = percent)) +
  facet_grid(type ~ ., scales = "free_y")

dat_perc %>% 
  ggplot(aes(x = year,
             y = percent,
             colour = percent)) +
  geom_smooth(aes(colour = ..y..),
              se = FALSE) +
  # geom_line() +
  geom_point() +
  scale_color_scico(palette = "roma",
                    direction = 1,
                    limits =  c(-fill_lim, fill_lim)) +
  # geom_area(aes(fill = percent)) +
  facet_grid(type ~ ., scales = "free_y") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey20"))

scico_roma <- scico(2, palette = "roma")

dat_perc %>% 
  ggplot(aes(x = year,
             y = percent,
             fill = percent)) +
  # geom_smooth(aes(colour = ..y..),
  #             se = FALSE) +
  # geom_line() +
  # geom_point() +
  geom_bar(stat = "identity") +
  scale_fill_scico(palette = "roma",
                   direction = 1,
                   limits =  c(-fill_lim, fill_lim)) +
  # scale_fill_gradient2(low = scico_roma[1],
  #                      high = scico_roma[2],
  #                      midpoint = 0) +
  # geom_area(aes(fill = percent)) +
  facet_wrap(facets = "type",
             # scales = "free_y",
             ncol = 1) +
  theme_void() +
  theme(#plot.background = element_rect(fill = "grey20"),
        panel.background = element_rect(fill = "grey20"),
        panel.grid = element_blank())


dat_perc %>% 
  filter(type == "butter") %>% 
  mutate(yend = lbs + (percent/10)) %>% 
  ggplot(aes(x = year,
             y = lbs)) +
  # geom_line(color = "grey80") +
  geom_segment(aes(yend = yend,
                   xend = ..x..,
                   colour = percent),
               size = 2,
               arrow = arrow(length = unit(1.2, "mm"),
                             type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  geom_text(aes(y = case_when(percent > 0 ~ yend + .12,
                              TRUE ~ yend - .12),
                label = percent %>% 
                  round() %>% paste0("%"),
                colour = percent),
            size = 3) +
  scale_colour_scico(palette = "roma",
                   direction = 1,
                   limits =  c(-fill_lim, fill_lim),
                   guide = FALSE) +
  guides(colour = element_blank()) +
  theme_bw()
 