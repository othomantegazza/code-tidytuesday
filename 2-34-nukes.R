library(tidyverse)
library(lubridate)
library(grid)
library(tibbletime)
library(units)

bg_col <- "#3752C3"
fill_col <- "#5867A6"

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-08-20/nuclear_explosions.csv")

data_path <- 'data/2-34-nukes.Rdata'


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
if(!file.exists(data_path)) {
  nukes <- 
    data_url %>% 
    read_csv(col_types = cols(
      date_long = col_datetime(format = "%Y%m%d")
    )) %>% 
    mutate(month = month(date_long))
  
  save(nukes, file = data_path)
} else {
  load(data_path)
}



# explore -----------------------------------------------------------------

nukes %>% 
  map(~is.na(.) %>% sum())

nukes %>% 
  ggplot(aes(x = date_long,
             y = magnitude_body)) +
  geom_point()


nukes %>% 
  ggplot(aes(x = date_long,
             y = magnitude_surface)) +
  geom_point()


nukes %>% 
  ggplot(aes(x = yield_upper,
             y = magnitude_body)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() 

nukes %>% pull(type) %>% unique

nukes %>% 
  mutate(u = case_when(depth >= 0 ~ TRUE, TRUE ~ FALSE)) %>% 
  ggplot(aes(x = year,
             fill = u)) +
  geom_bar()

  

# make smoothed spline ---------------------------------------------------------
# for cumulative plot


# every year for every country
# I need to merge it later to get a comprehensive dataset
year_states <- 
  expand.grid(year = min(nukes$year):max(nukes$year),
              country = unique(nukes$country), stringsAsFactors = FALSE)

# comprehensive dataset with row for every year/country
# and cumulative counts
nukes2 <- 
  nukes %>% 
  count(year, country) %>% 
  group_by(country) %>% 
  mutate(n_cumulative = cumsum(n)) %>% 
  select(-n) %>% 
  bind_rows(expand.grid(year = min(nukes$year) - 1,
              country = unique(nukes$country),
              n_cumulative = 0,
              stringsAsFactors = FALSE)) %>% 
  full_join(year_states) %>% 
  arrange(country, year) %>% 
  fill(n_cumulative, .direction = "down") %>% 
  # top 3 countries
  filter(country %in% c( "USA", "USSR", "FRANCE"))

tst <-nukes2 %>% 
  split(.$country) %>% 
  # map(~loess(formula = n_cumulative ~ year, data = ., span = .2))
  map(~smooth.spline(.$year, .$n_cumulative))

predict(object = tst$USA, x = 1960, deriv = 1)


# function to place text on path ------------------------------------------

plot_ratio <- .03

make_text_coords <- function(label = "CROSTATA AI MIRTILLI CON PANNA MONTATA ALLE 3",
                             smooth_obj = tst$USA,
                             start_at = median(yrs))
  {
  label <-  strsplit(label, split = "")[[1]]
  
  yrs <- nukes$year %>% unique()
  
  # approximate at pair number
  n <- length(label)/2 + length(label)%%2 
  
  at_x <- rep(start_at, length.out = n + 1 - length(label)%%2)
  
  # forward steps
  for(i in 2:length(at_x)) {
    # slope - adjusted for xy plot ratio
    slop <- (predict(object = smooth_obj, x = at_x[i - 1], deriv = 1)$y)*plot_ratio
    # distance  between the points, is cosine of slope angle
    dist_factor <- sin(atan(slop))/slop
    # update
    at_x[i] <- at_x[i - 1] + dist_factor
  }
  
  
  at_x_back <- rep(start_at, length.out = n)
  # back steps
  for(i in 2:length(at_x_back)) {
    # slope - adjusted for xy plot ratio
    slop <- (predict(object = smooth_obj, x = at_x_back[i - 1], deriv = 1)$y)*plot_ratio
    # distance  between the points, is cosine of slope angle
    dist_factor <- sin(atan(slop))/slop
    # update
    at_x_back[i] <- at_x_back[i - 1] - dist_factor
  }
  
  # put together
  at_x <- c(rev(at_x_back[-1]), at_x)
  
  tibble(label = label,
         year = at_x) %>% 
    # cumulative points
    mutate(n_cumulative = predict(object = smooth_obj, x = year)$y,
           # first derivative, need it for letter spacing and angles
           d1 = predict(object = smooth_obj, x = year, deriv = 1)$y,
           # angle is arctangent of 1st derivative
           # need to correct for xy ratio
           angle_rad = atan(d1*plot_ratio),
           angle =  angle_rad %>% as_units("radians") %>%
             set_units("degrees"))
  
}


text_data <- make_text_coords(#label = "ciao",
                              smooth_obj = tst$USA,
                              start_at = 1975) #nukes$year %>% unique() %>% median())


# plot cumulative smoothed spline and text --------------------------------



nukes3 <- 
  tibble(year = nukes2 %>% filter(country == "USA") %>% pull(year)) %>% 
  mutate(n_cumulative = predict(object = tst$USA, x = year)$y,
         d1 = predict(object = tst$USA, x = year, deriv = 1)$y)

nukes3 %>% 
  ggplot(aes(x = year,
             y = n_cumulative)) +
             #colour = country)) +
  geom_line(colour = "grey") +
  geom_line(aes(y = d1), colour = "red") +
  geom_text(data = text_data,
            aes(label = label,
                angle = angle),
            vjust = 0,
            hjust = .5,
            family = "courier") +
  coord_fixed(ratio = plot_ratio) +
  theme_bw()
