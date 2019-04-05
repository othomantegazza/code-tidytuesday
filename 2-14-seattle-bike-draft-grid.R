library(grid)

length(p_list)%/%(8*4)
# 13
length(p_list)%/%4
# 104

# outer margin left and right
m_side <- .15
# outer margin up and down
m_tb <- .2
# inner margin left and right
m_small_side <- .05
# inner margin top and bottom
m_small_tb <- .05

# must fit a group of 8 plots in half table width
p_width <- (.5 - m_side - m_small_side)/8 

# must 13 plots in half table height
p_height <- (.5 - m_tb - m_small_tb)/13

# directly in SVG

# in a data frame?
p_tibble <- 
  tibble(p = p_list[1:104],
         x = seq(from = m_side, to = .5 - m_small_side, length.out = 9)[1:8] %>% 
           rep(13),
         y = seq(from = 1 - m_tb, to = .5 + m_small_tb, length.out = 13) %>% 
           rep(each = 8),
         width = p_width,
         height = p_height)

plot_to_vp <- function(p, x, y, width, height) {
  print(p,
        vp = viewport(x = x, y = y, width = width, height = height))
  return(NULL)
}


# Very slow on r graphic devices ------------------------------------------

# grid.newpage()
# 
# p_tibble %>% pmap(plot_to_vp)


# much faster on SVG ------------------------------------------------------



svglite::svglite(file = "plots/2-14-seattle-bikes-draft-grid.svg")
grid.newpage()
p_tibble %>% pmap(plot_to_vp)
dev.off()
