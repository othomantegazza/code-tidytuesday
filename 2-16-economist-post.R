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
                  label = paste0(".", value*100),
                  colour = value),
              size = 10,
              fontface = "bold") +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(begin = .1, end = .9, guide = FALSE) +
    scale_colour_viridis_c(guide = FALSE, limits = c(0, 1)) +
    lims(x = c(-.5, 1.5)) +
    theme_void()
  
  return(p)
}

dat_plots <- 
  dat %>% 
  mutate(percent_women = as.numeric(percent_women)) %>%
  arrange(country, field) %>% 
  mutate(plots = percent_women %>% map(plot_circle))

dat_plots %>% pull(plots) %>% .[[1]]

# define grid -------------------------------------------------------------

s_margin <- .1
u_margin <- .2

length(p_list)
# 60

y_n <- dat_plots %>% pull(country) %>% unique() %>% length()
# 12

x_n <- dat_plots %>% pull(field) %>% unique() %>% length()
# 5

# a grid 12 x 5

width <- (1 - 2 * s_margin) / x_n
height <- (1 - 2*u_margin) / y_n

# plot on grid ------------------------------------------------------------

plot_to_vp <- function(p, x, y, width, height) {
  print(p,
        vp = viewport(x = x, y = y, width = width, height = height))
  return(NULL)
}

grid.newpage()


svglite::svglite(file = "plots/2-16-economist-post.svg",
                 height = 32,
                 width = 15)
dat_plots %>% 
  mutate(x = seq(s_margin, 1 - s_margin, length.out = x_n + 1)[1:x_n] %>% rep(y_n),
         y = seq(1 - u_margin, u_margin, length.out = y_n) %>% rep(each = x_n)) %>% 
  transmute(p = .data$plots,
            x = .data$x,
            y = .data$y,
            width = width,
            height = height) %>% 
  pmap(plot_to_vp)
dev.off()
