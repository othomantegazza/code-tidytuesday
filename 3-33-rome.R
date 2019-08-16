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


# explore -----------------------------------------------------------------

emps %>% 
  ggplot(aes(x = reorder(name, reign_start))) +
  geom_segment(aes(y = reign_start,
                   yend = reign_end,
                   xend = name)) +
  coord_flip()

emps %>% count(cause)


# try to reproduce the exact plot in grid ---------------------------------

# prepare data ------------------------------------------------------------

emps_toplot <- 
  emps %>% 
  mutate(reign_start = case_when(name == "Augustus" ~ as_date("0000-01-01"),
                                 TRUE ~ reign_start)) %>% 
  mutate(year = year(reign_start),
         month = month(reign_start)) %>% 
  filter(year <= 100) %>% 
  select(year, month, name)

# parameters - colors -----------------------------------------------------

bg_grey <- "#838798"
rect_fill <- "red"

colors_from_template <- c("#ED6B7C", "#F1C232", "#6AA94F", "#EFEFEF", "#6D9EEB")
bg_from_template <- "#434343"

# parameters - grid -------------------------------------------------------

height  <- 10; width <- 30
margin_left <- .3; margin_right <- .03; margin_side <- margin_left + margin_right
# n_long <- 10
# by_long <- 10
# rect_margin <- .4

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

# bar height

bar_height <- .08

bar_gap <- bar_height*.1

# associate emperor to color --------------------------------------------

emp_color <- 
  emps_toplot %>% 
  {set_names(nm = .$name,
             # x = sample(ochRe::ochre_palettes$olsen_seq, size = nrow(.), replace = T))}
             x = rep(colors_from_template, length.out = nrow(.)))}

# select color
make_gpar <- function(name) {
  gpar(fill = emp_color[name], col = "#00000000")
}


# add double years --------------------------------------------------------
# 
# emps_endyear <- 
#   emps %>% 
#   mutate(year = year(reign_end)) %>% 
#   filter(year <= 100) %>% 
#   select(year, name)
# 
# emps_toplot %>% 
#   rbind(emps_endyear) %>% 
#   distinct() %>% View()


# transition years --------------------------------------------------------

get_transition_year <- rollify(.f = function(i) i[1] != i[2], window = 2)
# is_last_year <- rollify(.f = function(i) i, window = 2)


# map data to rectangles ---------------------------------------------------

shapes_basic <-
  tibble(year = 0:99,
         # x = {seq(from = margin_left,
         #         to = 1 - margin_right,
         #         by = (1  - margin_side)/n_long) + ((1 - margin_side)/n_long)*rect_margin} %>% .[1:100],
         x = rect_x,
         y = .9,
         # width = ((1 - margin_side)/n_long)*(1 - rect_margin),
         width = rect_width,
         height = bar_height,
         vjust = 1) %>% 
  left_join(emps_toplot, by = "year") %>% 
  fill(name, .direction = "down") %>% 
  mutate(gp = map(name, make_gpar)) %>%
  mutate(transition = get_transition_year(name)) %>% 
  fill(transition, .direction = "up") %>% 
  mutate(last_year = c(transition[2:n()], transition[1]))
  # select(-name, -year)


# add transition years ----------------------------------------------------

# (more than one succession)
myears <- 
  shapes_basic %>% 
  count(year, sort = T) %>% 
  filter(n > 1) %>% 
  pull(year)

# half bar for start
shapes_transition <- 
  shapes_basic %>%
  filter(transition) %>%
  filter(!year %in% myears) %>% 
  mutate(y = y - 1/2*bar_height - bar_gap,
         height = 1/2*bar_height - bar_gap)

# half bar for end reign
shapes_last_year <- 
  shapes_basic %>% 
  filter(last_year) %>% 
  filter(!year %in% myears) %>%
  mutate(year = year + 1,
         height = 1/2*bar_height - bar_gap) %>% 
  select(-x) %>% 
  left_join(shapes_basic %>% select(year, x)) %>% 
  distinct(name, .keep_all = T)

# merge together

shapes_double <- 
  shapes_basic %>% 
  filter(!transition) %>% 
  rbind(shapes_transition) %>% 
  rbind(shapes_last_year) %>% 
  arrange(year) 


# more than 2 in a year ---------------------------------------------------

multi_year <- rbind(shapes_basic %>% filter(year %in% myears),
                    shapes_double %>% filter(year %in% myears))



# remove extra columns ----------------------------------------------------

shapes_df <- 
  shapes_double %>% # prepare for plots
  select(-name, -year, -transition, -last_year, -month)
  

# plot everything in svg --------------------------------------------------


svglite::svglite("plots/2-33-rome.svg",
                 width = width,
                 height = height)

# new page, is it necessary?
grid.newpage()

# background grey rectangle
grid.rect(gp = gpar(fill = bg_from_template, col = bg_from_template))
          
# grid.rect(x = .1, y = .9, width = .1, height = .1, vjust = .5, hjust = .5)

shapes_df %>% 
  pmap(grid.rect)
dev.off()

