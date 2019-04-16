library(tidyverse)
library(rlang)
library(grid)

# Get and clean data ------------------------------------------------------


dat_url <- paste0("https://raw.githubusercontent.com/",
              "rfordatascience/tidytuesday/master/data/",
              "2019/2019-04-16/Economist_women-research.csv")


dat_path <- "data/2-16-economist-post.Rdata"

if(!file.exists(dat_path)) {
  dat_raw <- readr::read_csv(dat_url)
  
  research_names <- c("country",
                      "Health sciences",
                      "Physical sciences",
                      "Engineering",
                      "Computer science, maths",
                      "Women inventores")
  
  dat <- 
    dat_raw %>% 
    na.omit() %>% 
    set_names(research_names) %>% 
    filter(country != "Country") %>% 
    gather(field, percent_women, `Health sciences`:`Women inventores`)
    
  
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# loop plots --------------------------------------------------------------

plot_circle <- function(percent_women) {
  p <- 
    tibble(gender = c("female", "male") %>% factor(levels = c("male", "female")),
           value = c(percent_women, 1 - percent_women)) %>%  
    ggplot() +
    geom_bar(aes(x = 1,
                 y = value,
                 fill = gender),
             stat = "identity") + 
    geom_text(data = . %>% 
                filter(gender == "female"),
              aes(x = -.5, y = 0,
                  label = value,
                  colour = value),
              size = 10) +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(begin = .1, end = .9, guide = FALSE) +
    scale_colour_viridis_c(guide = FALSE, limits = c(0, 1)) +
    lims(x = c(-.5, 1.5)) +
    theme_void()
  
  return(p)
}

p_list <- 
  dat %>% 
  mutate(percent_women = as.numeric(percent_women)) %>% 
  pull(percent_women) %>% 
  map(plot_circle)

p_list[[40]]


# define grid -------------------------------------------------------------

s_margin <- .1
u_margin <- .2


length(p_list)
