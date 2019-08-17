## TRY TO REPRODUCE THIS PLOT:
## https://www.reddit.com/r/dataisbeautiful/comments/8tzfgz/roman_emperors_by_year_oc/
## IN R GRID

library(tidyverse)
library(lubridate)
library(grid)
library(tibbletime)

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2019/2019-08-13/emperors.csv")

data_path <- "data/2-33-rome.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  emps <- 
    data_url %>% 
    read_csv() %>% 
    janitor::clean_names()
  
  save(emps, file = data_path)
} else {
  load(data_path)
}


# simplify timeline -------------------------------------------------------

# reomove co-reign 
# keep main emperors or emperors from the Rome province

out <- c("Lucius Verus",
         "Geta",
         "Gordian II",
         "Pupienus",
         "Hostilian",
         "Numerian",
         "Maximian",
         "Galerius",
         "Severus II",
         "Maxentius",
         "Maximinus II",
         "Lucinius I",
         "Constantine II",
         "Constans",
         "Vetranio",
         "Valens",
         "Gratian")

# try to reproduce the exact plot in grid ---------------------------------

# prepare data ------------------------------------------------------------

emps_simple <- 
  emps %>% 
  mutate(reign_start = case_when(name == "Augustus" ~ as_date("0001-01-01"),
                                 TRUE ~ reign_start)) %>% 
  mutate(year = year(reign_start),
         month = month(reign_start)) %>% 
  # filter(year <= 100) %>% 
  select(year, month, name) %>% 
  filter(! name %in% out)

last_year <- emps$death %>% year() %>% max()

# parameters - colors -----------------------------------------------------

bg_grey <- "#838798"
rect_fill <- "red"

colors_from_template <- c("#ED6B7C", "#F1C232", "#6AA94F", "#EFEFEF", "#6D9EEB")
bg_from_template <- "#434343"

# associate emperor to color --------------------------------------------

# all names are unique
emp_color <- 
  emps_simple %>% 
  {set_names(nm = .$name,
             x = rep(colors_from_template, length.out = nrow(.)))}

# select color
make_gpar <- function(name) {
  gpar(fill = emp_color[name], col = "#00000000")
}


# detect transition years and years with multiple emperors ----------------

get_transition_year <- rollify(.f = function(i) i[1] != i[2], window = 2)

emps_toplot <- 
  tibble(year = 1:last_year) %>% 
  left_join(emps_simple, by = "year") %>% 
  fill(name, .direction = "down") %>% 
  # transition years
  mutate(transition = get_transition_year(name)) %>% fill(transition, .direction = 'up') %>%
  mutate(new_year = get_transition_year(year)) %>% 
  # multiple years
  left_join(emps_simple %>% count(year), by = "year") %>%
  # detect end year
  mutate(last_year = case_when(transition & new_year ~ n)) %>% mutate(last_year = last_year[c(2:n(), 1)])


# add a record for the year in which each emperor loses its trone
emps_toplot <-
  emps_toplot %>%
  bind_rows(emps_toplot %>% filter(last_year > 0) %>% mutate(year = year + 1, n = last_year, month = 0)) %>%
  arrange(year, month) %>%
  mutate(n = case_when(year == 1 ~ as.integer(0), TRUE ~ n))

# parameters - grid -------------------------------------------------------

height  <- 10; width <- 30
margin_left <- .3; margin_right <- .03; margin_side <- margin_left + margin_right


# size of block of ten years
block_size <- (1 - margin_side)/10

# inner margin between ten years bloks
block_margin <- block_size/12

# rectangle width
rect_width <- ((block_size - block_margin) / 10) * 0.62

# x position of bars within blocks
inner_position <- seq(0,
                      block_size - block_margin - rect_width,
                      length.out = 10)

# all x positions (100)
rect_x_blocks <- seq(from = margin_left, by = block_size, length.out = 10) %>% rep(each = 10)
rect_x_small <- rep(inner_position, 10) 

rect_x  <- rect_x_blocks + rect_x_small

# function that returns x position

get_x <- function(year) {rect_x[ (year %% 100) %>% {case_when(. == 0 ~ 100, TRUE ~ .)} ]  }

# bar height for years with multiple emperors -----------------------------

bar_height <- .08

bar_gap <- bar_height*.1

# four rows
bar_y <- seq(.9, .4, length.out = 4)

# function that returns y position
get_y <- function(year) {bar_y[ (year %/% 100) + 1 ]}

# make tibble with rectangle shapes --------------------------------------


shapes_basic <- 
  emps_toplot %>% 
  mutate(x = get_x(year),
         y = get_y(year),
         width = rect_width,
         height = bar_height,
         gp = map(name, make_gpar),
         vjust = 1) 

# fix years with multiple transitions -------------------------------------

years_multiple <- which(shapes_basic$n > 0)

fix_shapes <- function(df)
{
  n_changes <- nrow(df)
  
  ydiff <- seq(0, bar_height + bar_gap, length.out = n_changes + 1)[1:(n_changes)]
  
  df %>% 
    mutate(height = (height + bar_gap)/(n_changes) - bar_gap,
           y = y - ydiff)
}

shapes_multiple <- 
  shapes_basic[years_multiple, ] %>% 
  split(.$year) %>% 
  map(fix_shapes) %>% 
  reduce(bind_rows)

shapes_basic <-
  bind_rows(shapes_basic[-years_multiple, ], shapes_multiple) %>% 
  arrange(year, month)


# remove extra columns ----------------------------------------------------

shapes_df <- 
  shapes_basic %>% 
  select(-year, -month, -name, -transition, -new_year, -n, -last_year)
  

# plot everything in svg --------------------------------------------------


svglite::svglite("plots/2-33-rome-2.svg",
                 width = width,
                 height = height)

# new page, is it necessary?
grid.newpage()

# background grey rectangle
grid.rect(gp = gpar(fill = bg_from_template, col = bg_from_template))
          
shapes_df %>% 
  pmap(grid.rect)
dev.off()

