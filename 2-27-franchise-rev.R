library(tidyverse)
library(packcircles) ## easily package circles
library(ggforce)
library(grid)

bg_col <- "#F6F6DF" # "#E4F3F9"

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

# 1. rank categories and estimate revenue ratios --------------------------

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

# 2. set colors -----------------------------------------------------------
colorz <-
  c("#D9F6FF", # "#EAEA9F", 
    "#E97E00","#E0E0D2","#5867A6", # "#44D4FF", "#E2CD92",
    "#27A6D3", "#1E2D6C","#CD0A62","#CB7BA5") %>%
  set_names(nm = fr %>% pull(revenue_category) %>% unique())

# colorz <- 
#   Redmonder::redmonder.pal(n = 8, name = "qMSOPu2")[2:8] %>% 
#   c("#27A6D3") %>% 
#   set_names(nm = fr %>% pull(revenue_category) %>% unique())


# colorz <-
#   c("#D81B60","#8E24AA","#5E35B1","#3949AB","#1E88E5","#039BE5","#00ACC1","#00897B") %>%
#   set_names(nm = fr %>% pull(revenue_category) %>% unique())

# 3. try plot first rank --------------------------------------------
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
  theme_void() +
  theme(legend.position = "right",
        legend.justification = "center") +
  guides(colour = FALSE,
         fill = FALSE) 

p

# 4. make a list of ranks and loop over it -----------------------------------

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


# 5. make legend maually --------------------------------------------------

leg <- 
  tibble(y = rep(c(1,2), each = 4),
         x = rep(1:4, times = 2),
         fill = colorz %>% names(),
         r = .1) %>% 
  ggplot() +
  geom_text(aes(x = x, y = y - .26,
                label = str_wrap(fill, 15)),
            lineheight = .9,
            size = 1.2,
            vjust = 1, family = "courier", fontface = "bold") +
  geom_circle(aes(x0 = x,
                  y0 = y,
                  fill = fill,
                  colour = fill,
                  r = r)) +
  guides(fill = FALSE, colour = FALSE) +
  scale_fill_manual(values = colorz) +
  scale_colour_manual(values = colorz) +
  coord_equal() +
  theme_void() +
  lims(y = c(0.5, 2.3),
       x = c(.5, 4.5))

leg

# 6. save plot ------------------------------------------------------------



png(filename = "plots/2-27-franchise-rev.png",
    height = 1600,
    width = 2700,
    res = 400)
grid.newpage()
grid.rect(gp = gpar(fill = bg_col)) # background
p %>% print(vp = viewport(x = .42, y = .5)) # plot
leg %>% print(vp = viewport(x = .83, y = .16, width = .26)) # legend
grid.text(label = "Media Franchise Revenues", # title
          x = .98,
          y = .3,
          hjust = 1,
          gp = gpar(fontfamily = "courier",
                    col = "#100089",
                    fontsize = 10.5,
                    fontface = "bold"))
grid.text(label = "Data by Wikipedia | Plot by @othomn", # caption
          x = .02, y = .03, 
          hjust = 0,
          gp = gpar(fontfamily = "courier",
                    col = "#AA2255",
                    # fontface = "bold",
                    fontsize = 4))
dev.off()
