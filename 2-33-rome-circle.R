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

# simplify dataset --------------------------------------------------------

# and fix BC date (only 1: Augustus start)

diffday <- (emps$reign_start[1] - as_date("0001-01-01")) %>% as.numeric()

emps2 <- 
  emps %>% 
  mutate(reign_start = case_when(name == "Augustus" ~ (reign_start - 2*diffday),
                                 TRUE ~ reign_start)) %>%
  select(reign_start, reign_end, name)



# grid parameters ---------------------------------------------------------

width  <- 15; height <- 40

margin_top <- .2; margin_low <- .1

bg_col <- "#3752C3"

x_labels <- .36

# Turn all dates to numeric? ----------------------------------------------

# to map them into the x space


range_reign <- c(min(emps2$reign_start), max(emps2$reign_end))

rescale_reing <- function(y)
  {
  scales::rescale(y,
                  to = c(1 - margin_top, margin_low),
                  from = range_reign)
  }

emps3 <- 
  emps2 %>% 
  mutate(start_y = reign_start %>% rescale_reing(),
         end_y = reign_end %>% rescale_reing(),
         r = (start_y - end_y)/2 ,
         y = start_y - r,
         r = r * (height/width) * .75,
         x = .7)


to_circles <-
  emps3 %>% 
  select(x, r, y) %>% 
  mutate(gp = list(gpar(fill = "#ffffff66", col = "#ffffff00")))


# labels ------------------------------------------------------------------

emps4 <-
  emps3 %>%
  arrange(reign_start, reign_end) %>%
  mutate(n = 1:n(),
         ylab = scales::rescale(n, from = range(n), to = c(1 - margin_top, margin_low)),
         gp = list(gpar(size = 10, col = "white")),
         xlab = x_labels,
         rot = 0,
         hjust = 1)

to_label <- 
  emps4 %>%
  select(label = name,
         y = ylab,
         x = xlab,
         gp, rot, hjust)



# bezier curves -----------------------------------------------------------

max_r <- emps4$r %>% max()

xbez_stop <-  1 - x_labels - max_r



make_bez_x <- function(xlab) {c(xlab, xbez_stop, xlab, xbez_stop)}
make_bez_y <- function(ylab, y) {c(ylab, ylab, y, y)}

bezier_y_in <- emps4 %>% select(ylab, y) %>% pmap(make_bez_y)


emps5 <- 
  emps4 %>% 
  mutate(bezier_x = xlab %>% map(make_bez_x),
         bezier_y = bezier_y_in) 

to_bezier <- 
  emps5 %>% 
  select(x = bezier_x,
         y = bezier_y)

# plot --------------------------------------------------------------------

svglite::svglite("plots/2-33-rome-circle.svg",
                 width = width,
                 height = height)

# new page, is it necessary?
grid.newpage()

# background blue rectangle
grid.rect(gp = gpar(fill = bg_col, col = bg_col))

# draw circles
to_circles %>%
  pmap(grid.circle)

# draw labels
to_labels %>%
  pmap(grid.text)

# draw_beziers
to_bezier %>% 
  pmap(grid.bezier)

# point at reign start
# emps3 %>%
#   select(x = start_x) %>%
#   mutate(r = .001,
#          gp = list(gpar(col = "white"))) %>%
#   pmap(grid.circle)


dev.off()




