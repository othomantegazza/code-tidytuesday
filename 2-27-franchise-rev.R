library(tidyverse)
library(packcircles) ## easily package circles
library(ggforce)

# get data ----------------------------------------------------------------

data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

data_path <- "data/2-27-franchise-rev.Rdata"


if(!file.exists(data_path)) {
  fr <- 
    data_url %>% 
    read_csv() %>% 
    distinct()
  
  save(fr, file = data_path)
} else {
  load(data_path)
}


# explore -----------------------------------------------------------------

# biggest revenues from merchandise
fr %>% 
  arrange(desc(revenue))

# revenue vs year
fr %>% 
  ggplot(aes(x = year_created,
             y = revenue,
             colour = revenue_category)) +
  geom_point()


# plot packaged circles ---------------------------------------------------

# Inspired by:
# https://chichacha.netlify.com/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/

# total revenue by franchise
tot_rev <- 
  fr %>%
  group_by(franchise) %>% 
  summarise(tot_revenue = sum(revenue)) %>% 
  arrange(desc(tot_revenue))

# pack circles
set.seed(93) # use motogp riders to select seeds ;)
packs <- 
  tot_rev %>% 
  pull(tot_revenue) %>% 
  # circleProgressiveLayout()
  circleRepelLayout() %>% 
  .$layout


# and check if you can save circles in a tibble
# with extra variables
packs_df <- 
  bind_cols(tot_rev, packs) %>% 
  select(x, y, radius, franchise) %>% 
  # the column id is needed later to join 
  # the dataset from circleLayoutVertices
  mutate(id = 1:n())


# plot with circlelayout
packs_df %>% 
  circleLayoutVertices(npoints = 200) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group=id)) +
  coord_equal() +
  theme_void()
 
# plot with ggforce
# easier
packs_df %>% 
  ggplot() +
  geom_circle(aes(x0 = x,
                  y0 = y, 
                  r = radius),
              fill = "#CD0A39",
              colour = "#CD0A39") +
  coord_equal()

# inner circles -----------------------------------------------------------

# 1. rank categories and estimate revenue ratios

fr_ranked <- 
  fr %>% 
  group_by(franchise) %>%
  arrange(franchise, desc(revenue)) %>% 
  mutate(rank = 1:n(),
         rev_tot = sum(revenue),
         rev_ratio = revenue/rev_tot) %>% 
  arrange(franchise, revenue) %>% 
  # cummulative revenue from lowest
  mutate(cumul_ratio = cumsum(rev_ratio))

# 2. set colors
colorz <- 
  c("#D9F6FF","#E97E00","#E0E0D2","#5867A6","#100089","#1E2D6C","#CD0A62","#CB7BA5") %>% 
  set_names(nm = fr %>% pull(revenue_category) %>% unique())

# 3. try plot first rank 
#  with ggforce geom_circle

pack1 <- 
  packs_df %>% 
  left_join(fr_ranked %>% filter(rank == 1), by = "franchise")

p <- 
  pack1 %>% 
  ggplot() +
  geom_circle(aes(x0 = x,
                  y0 = y, 
                  r = radius,
                  fill = revenue_category,
                  colour = revenue_category),
              size = .1) +
  scale_fill_manual(values = colorz) +
  scale_colour_manual(values = colorz) +
  coord_equal() +
  theme_void()

# 4. make a list of ranks and loop over it

get_rank_circles <- function(rank_in = 2) {
  packs_n <- 
    packs_df %>% 
    left_join(fr_ranked %>% filter(rank == rank_in), by = "franchise") %>% 
    # scale radius on proportion to cumulative revenue
    mutate(radius = radius*cumul_ratio)
}

circles_dfs <- map(2:max(fr_ranked$rank), get_rank_circles)


for(i in circles_dfs) {
  p <-
    p +
    geom_circle(data = i,
                aes(x0 = x,
                    y0 = y, 
                    r = radius,
                    fill = revenue_category,
                    colour = revenue_category),
                size = .1)
} 

png(filename = "plots/2-27-franchise-rev.png",
    height = 1300,
    width = 2000,
    res = 400)
p
dev.off()
