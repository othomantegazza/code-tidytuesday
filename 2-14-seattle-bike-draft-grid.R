library(tidyverse)
library(lubridate)
library(grid)


# weekdays in english
Sys.setlocale("LC_TIME", "en_US.UTF8")

length(p_list)
# 418

length(p_list)%/%(8*4)
# 13
length(p_list)%/%4
# 104

# rows per block
block_rows <- 14
block_cols <- 8
# 112 cells per block

112*4
# 448 for 418 cells
# 30 cells too much
 

# outer margin left and right
m_side <- .14
# outer margin up and down
m_tb <- .16
# inner margin left and right
m_small_side <- .04
# inner margin top and bottom
m_small_tb <- .05

# must fit a group of 8 plots in half table width
p_width <- (.5 - m_side - m_small_side)/8 

# must 13 plots in half table height
p_height <- (.5 - m_tb - m_small_tb)/13


# background color
bg_col <- "#F1F1F0" #"#F2F2EF"#"#F6F6EC"

# in a data frame?
p_tibble <- 
  tibble(p = p_list,
         x = seq(from = m_side, to = .5 - m_small_side, length.out = 9)[1:block_cols] %>% 
           rep(block_rows) %>%
           # second block, right top
           c(seq(from = .5 + m_small_side, to = 1 - m_side, length.out = 9)[1:block_cols] %>% 
               rep(block_rows)) %>% 
           # # third and fourth blocks (bottom)
           {c(.,.)} %>% .[1:length(p_list)],
         y = seq(from = 1 - m_tb, to = .5 + m_small_tb, length.out = block_rows) %>% 
           rep(each = block_cols) %>% 
           # second block, right top
           {c(., .)} %>% 
           c(seq(from = .5 - m_small_tb, to = m_tb, length.out = block_rows) %>% 
               rep(each = block_cols) %>% 
               {c(., .)}) %>% 
           .[1:length(p_list)],
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



svglite::svglite(file = "plots/2-14-seattle-bikes-draft-grid.svg",
                 height = 33.1,
                 width = 23.4 )
grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
grid.text("365 Days on the Bicycle in Seattle",
          x = m_side,
          y = 1 - m_tb/2,
          gp = gpar(col = "grey20",
                    fontsize = 60,
                    fontface = "italic",
                    fontfamily = "Times New Roman"),
          hjust = 0,
          vjust = 0)
grid.text(str_wrap("Bicycle traffic crossing 7 detection points in Seattle in 2017.
                   Also an exercise on making heavily facetted plots with ggplot2 and
                   grid by Otho Mantegazza"),
          x = m_side,
          y = 1 - m_tb*.72,
          gp = gpar(col = "grey20",
                    fontsize = 30,
                    fontface = "italic",
                    fontfamily = "Times New Roman"),
          hjust = 0,
          vjust = 0)
grid.lines(x = unit(c(m_side*1.5, 1 - m_side*1.5), "npc"),
           y = unit(c(.5, .5), "npc"))
p_tibble %>% pmap(plot_to_vp)
dev.off()

png(filename = "plots/2-14-seattle-bikes-draft-grid.png", 
    height = 33.1,
    width = 23.4,
    units = "in",
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = bg_col))
p_tibble %>% pmap(plot_to_vp)

dev.off()
