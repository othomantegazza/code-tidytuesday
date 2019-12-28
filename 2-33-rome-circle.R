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

width  <- 18; height <- 40

margin_top <- .06; margin_low <- .04

bg_col <- "#3752C3"

x_labels <- .51

x_circle <- .77

x_title <- .12

y_sign <- margin_low + 1

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
         r = r * (height/width),
         x = x_circle)


to_circles <-
  emps3 %>% 
  select(x, r, y) %>% 
  mutate(gp = list(gpar(fill = "#ffffff66", col = "#ffffff00")))


# labels ------------------------------------------------------------------

emps4 <-
  emps3 %>%
  arrange(desc(y)) %>%
  mutate(n = 1:n(),
         ylab = scales::rescale(n, from = range(n), to = c(1 - margin_top, margin_low)),
         gp = list(gpar(fontsize = 12, col = "white")),
         xlab = x_labels,
         rot = 0,
         hjust = 1) %>% 
  mutate(ylab = (y + ylab)/2) %>% 
  mutate(reign_span = case_when(year(reign_start) == year(reign_end) ~ year(reign_start) %>% as.character(),
                                year(reign_start) < 0 ~ paste(paste(-year(reign_start), "BC"),
                                                            year(reign_end), sep = " - "),
                                TRUE ~ paste(year(reign_start), year(reign_end), sep = " - ")),
         label = paste(name, reign_span, sep = " | "))

to_label <- 
  emps4 %>%
  select(label,
         y = ylab,
         x = xlab,
         gp, rot, hjust)



# bezier curves -----------------------------------------------------------

max_r <- emps4$r %>% max()

xbez_stop <-  x_circle - max_r * 1.4



make_bez_x <- function(xlab) {c(xlab, mean(xbez_stop, x_labels)*0.87, mean(xbez_stop, xlab)*0.8, xbez_stop)}
make_bez_y <- function(ylab, y) {c(ylab, ylab, y, y)}
make_gpar <- function(lty) {gpar(col = "white", lwd = .5, lty = lty)}

bezier_y_in <- emps4 %>% select(ylab, y) %>% pmap(make_bez_y)
bezier_gpar <- rep(1:3, length.out = nrow(emps4)) %>% map(make_gpar)


emps5 <- 
  emps4 %>% 
  mutate(bezier_x = xlab %>% map(make_bez_x),
         bezier_y = bezier_y_in,
         gp = bezier_gpar)

to_bezier <- 
  emps5 %>% 
  select(x = bezier_x,
         y = bezier_y,
         gp) 


# segments ----------------------------------------------------------------

# connect beziers to circles

to_segments <- 
  emps5 %>% 
  transmute(x0 = xbez_stop,
            x1 = x - .02 - scales::rescale(r, from = range(r), to = c(median(r), max(r)*1.1)),
            y0 = y,
            y1 = y,
            gp = gp)


# signature ---------------------------------------------------------------

sig_x <- .15
sig_y <- .07
sig_r <- .013
sig_col <- "#CB7BA5"

make_circle_text <- function(label, x, y, r, angle) 
{
  angle_pi <- scales::rescale(angle, from = c(0, 360), to = c(0, 2*pi))
  list(label = label,
       x = x + sin(angle_pi)*r,
       y = y + cos(angle_pi)*r*(width/height),
       rot = -angle)
}

signature <- "plot by @othomn" %>% strsplit(split = "") %>% .[[1]]

to_signature <- 
  tibble(label = signature) %>% 
  mutate(n = 1:n(),
         angle = scales::rescale(n, from = range(n), to = c(-80, 80)),
         x = sig_x,
         y = sig_y,
         r = sig_r + .016) %>% 
  select(-n) %>% 
  pmap_df(make_circle_text) %>% 
  mutate(vjust = 0,
         hjust = 0.5,
         rot = rot - 1,
         gp = list(gpar(fontsize = 16,
                        col = sig_col,
                        fontfamily = "mono",
                        fontface = "bold")))

# plot --------------------------------------------------------------------
 
# svglite::svglite("plots/2-33-rome-circle.svg",
#                  width = width,
#                  height = height)

png("plots/2-33-rome-circle.png",
    width = width,
    height = height, units = "in", res = 300)

# new page, is it necessary?
grid.newpage()

# background blue rectangle
grid.rect(gp = gpar(fill = bg_col, col = bg_col))

# draw circles
to_circles %>%
  pmap(grid.circle)

# draw labels
to_label %>%
  pmap(grid.text)

# draw_beziers
to_bezier %>% 
  pmap(grid.bezier)

# connect them to circles with segments
to_segments %>% 
  pmap(grid.segments)


# add title
grid.text(label = str_wrap("Timeline of Roman Emperors", width = 6),
          x = x_title,
          y = (to_label %>% arrange(desc(y)) %>% pull(y) %>% .[1]) + .008,
          hjust = 0,
          vjust = 1,
          gp = gpar(fontsize = 40,
                    # fontface = "bold",
                    col = "#D8DDF3", #  "#98F0D8",  #  "#44D4DC", # "#F6F6DF", #
                    lineheight = 1))

# add signature
grid.circle(x = sig_x,
            y = sig_y,
            r = sig_r,
            gp = gpar(fill = sig_col,
                      col = sig_col,
                      alpha = .9))

to_signature %>% 
  pmap(grid.text)

# add source
grid.text("Data from Wikipedia.",
          x = x_title,
          y = margin_low + .005,
          hjust = 0,
          vjust = 0,
          gp = gpar(fontsize = 12,
                    fonttype = "mono",
                    col = "#D8DDF3"))


dev.off()


# save json var -----------------------------------------------------------

library(jsonlite)

emps %>% 
  toJSON() %>%
  {paste("var emps = ", .)} %>% 
  cat(file = "d3/json_data/2-33-rome.js")




