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

height  <- 5; width <- 40

margin_left <- .2; margin_right <- .1; margin_side <- margin_left + margin_right

bg_col <- "#3752C3"

# Turn all dates to numeric? ----------------------------------------------

# to map them into the x space


range_reign <- c(min(emps2$reign_start), max(emps2$reign_end))

rescale_reing <- function(x)
  {
  scales::rescale(x,
                  to = c(margin_left, 1 - margin_right),
                  from = range_reign)
  }

emps3 <- 
  emps2 %>% 
  mutate(start_x = reign_start %>% rescale_reing(),
         end_x = reign_end %>% rescale_reing(),
         r = (end_x - start_x)/2 ,
         x = end_x - r,
         r = r * (width/height) * .75)


to_circles <-
  emps3 %>% 
  select(x, r) %>% 
  mutate(gp = list(gpar(fill = "#ffffff66", col = "#ffffff00")))


# plot --------------------------------------------------------------------

svglite::svglite("plots/2-33-rome-circle.svg",
                 width = width,
                 height = height)

# new page, is it necessary?
grid.newpage()

# background blue rectangle
grid.rect(gp = gpar(fill = bg_col, col = bg_col))

to_circles %>%
  pmap(grid.circle)

# point at reign start
emps3 %>%
  select(x = start_x) %>%
  mutate(r = .001,
         gp = list(gpar(col = "white"))) %>%
  pmap(grid.circle)


dev.off()




