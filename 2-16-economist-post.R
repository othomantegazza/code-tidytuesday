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


# order country and fields ------------------------------------------------

dat <- 
  dat %>% 
  mutate(percent_women = as.numeric(percent_women),
         # order countries
         country = factor(country, levels = c("Portugal", "Brazil", "Australia",
                                               "Canada", "France", "EU28",
                                               "Denmark", "Mexico", "United States",
                                               "United Kingdom", "Chile", "Japan")),
         # order fields
         field = factor(field, levels = c("Women inventores", "Computer science, maths",
                                          "Engineering", "Physical sciences",
                                          "Health sciences"))) %>%
  arrange(country, field) 

# define grid parameters ----------------------------------------------------

bg <- "white"
text_color <- "grey20"

left_margin <- .3
right_margin <- .1
u_margin <- .2

nrow(dat)
# 60

y_ids <- dat %>% pull(country) %>% levels() 
y_n <- y_ids  %>% length()
# 12

x_ids <- dat %>% pull(field) %>% levels()
x_n <- x_ids  %>% length()
# 5

# a grid 12 x 5

width <- (1 - left_margin - right_margin) / x_n
height <- (1 - 2*u_margin) / y_n

p_font <- "Helvetica"  #"Times New Roman"
p_fontface <- "plain"
p_fontsize <- 22

# loop plots --------------------------------------------------------------

plot_circle <- function(percent_women, xdown = -1) {
  p <- 
    tibble(gender = c("female", "male") %>% factor(levels = c("male", "female")),
           value = c(percent_women, 1 - percent_women)) %>%  
    ggplot() +
    geom_bar(aes(x = 1,
                 y = value,
                 fill = gender),
             stat = "identity",
             colour = bg,
             size = 2) + 
    geom_text(data = . %>% 
                filter(gender == "female"),
              aes(x = xdown, y = 0,
                  label = paste0(".", value*100),
                  colour = value),
              size = 10,
              fontface = "italic") +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(begin = .1, end = .9, guide = FALSE) +
    scale_colour_viridis_c(guide = FALSE, limits = c(0, 1)) +
    lims(x = c(xdown, 1.45)) +
    theme_void() +
    theme(plot.margin = margin(0,0,0,0, unit = "in"))
  
  return(p)
}

dat_plots <- 
  dat %>% 
  mutate(plots = percent_women %>% map(~plot_circle(., xdown = -.9)))

dat_plots %>% pull(plots) %>% .[[2]]



# Utility functions -------------------------------------------------------

add_fields <- function(x, label) {
  grid.text(label = label %>% str_wrap(width = 10),
            x = x,
            y = 1 - u_margin*.85,
            vjust = 0,
            gp = gpar(col = grey20,
                      fontsize = p_fontsize,
                      fontface = p_fontface,
                      fontfamily = p_font))
  return(NULL)
}

add_countries <- function(y, label) {
  grid.text(label = label,
            x = left_margin - .07,
            y = y,
            vjust = 0,
            hjust = 1,
            gp = gpar(col = text_color,
                      fontsize = p_fontsize,
                      fontface = p_fontface,
                      fontfamily = p_font))
  return(NULL)
}


# plot on grid ------------------------------------------------------------

plot_to_vp <- function(p, x, y, width, height) {
  print(p,
        vp = viewport(x = x, y = y, width = width, height = height))
  return(NULL)
}

svglite::svglite(file = "plots/2-16-economist-post.svg",
                 height = 32,
                 width = 15)
# field
tibble(x = seq(left_margin, 1 - right_margin, length.out = x_n + 1)[1:x_n],
       label = x_ids) %>% 
  pmap(add_fields)

# countries
tibble(y =  seq(1 - u_margin, u_margin, length.out = y_n),
       label = y_ids) %>% 
  pmap(add_countries)

# plots in grid
dat_plots %>% 
  mutate(x = seq(left_margin, 1 - right_margin, length.out = x_n + 1)[1:x_n] %>% rep(y_n),
         y = seq(1 - u_margin, u_margin, length.out = y_n) %>% rep(each = x_n)) %>% 
  transmute(p = .data$plots,
            x = .data$x,
            y = .data$y,
            width = width,
            height = height) %>% 
  pmap(plot_to_vp)
dev.off()

