# Most steps are taken form:
# https://chichacha.netlify.com/2018/11/12/utilizing-k-means-to-extract-colours-from-your-favourite-images/

library(tidyverse)
library(imager)
library(ggvoronoi)
library(grid)

bg_color <- "#E8EDEF"  

# function to plot voronoi 
img_to_voronoi <- function(img,
                           n_pix = 2500,
                           border_size = .2,
                           pick_by = NULL,
                           bg_color = "#E8EDEF",
                           delaunay = FALSE,
                           delaunay_tile = FALSE,
                           grays = TRUE) {
  
  pick_by <- enquo(pick_by)

  # hex value for every pixel
  hex_pix <- 
    img  %>% 
    as.data.frame(wide = "c") %>% 
    mutate(hexval = rgb(c.1,c.2,c.3))
  

  # luminosity for every pixes
  if(grays) {grey_pix <- 
    img %>% 
    grayscale() %>% 
    as.data.frame()

  # merge
  hex_pix <- 
    hex_pix %>%
    inner_join(grey_pix)}
  
  # p <- ggplot(hex_pix,aes(x,y))+geom_raster(aes(fill=hexval))+scale_fill_identity()
  # p
  
  set.seed(63); hex_pix_mini <- 
    hex_pix %>% 
    sample_n(n_pix,
             weight = !!pick_by
    ) 
  
  pix_colors <- 
    hex_pix_mini %>% 
    pull(hexval) %>% 
    {purrr::set_names(x = .,
                      nm = .)}

  range_x <- c(0, dim(img)[1])
  range_y = c(dim(img)[2], 0)
  
  p <- 
    hex_pix_mini %>% 
    ggplot(aes(x = x,
               y = y)) +
    # geom_point(aes(colour = hexvalue)) +
    {if(!delaunay_tile) {
      ggvoronoi::geom_voronoi(aes(fill = hexval),
                            size = border_size,
                            colour = bg_color)
      }} +
    {if(delaunay) {
      ggforce::geom_delaunay_segment(colour = "grey80",
                                                 size = .2,
                                                 linetype = 2)
      }} +
    {if(delaunay_tile) {
      # ggforce::geom_delaunay_tile(
      #   # aes(fill = hexval),
      #   colour = "white")
      ggforce::geom_delaunay_segment2(aes(colour = hexval,
                                          group = -1),
                                      size = border_size,
                                      lineend = "round")
      }} +
    scale_y_reverse(limits = range_y) +
    scale_fill_manual(values = pix_colors, guide = FALSE) +
    {if(delaunay_tile) scale_colour_manual(values = pix_colors, guide = FALSE)} +
    coord_fixed() +
    theme_void() +
    lims(x = range_x)
  
  return(p)
}


# img 2 -------------------------------------------------------------------

img <- load.image("~/Downloads/img2.jpeg")


p <- img_to_voronoi(img = img,
                    n_pix = 2500,
                    pick_by = c.3,
                    border_size = 0)

p

png("plots/2-32-img2-voronoi.png",
    width = 1700,
    height = 3000,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = bg_color))
p %>% print(vp = viewport())
dev.off()


p <- img_to_voronoi(img = img,
                    n_pix = 10000,
                    pick_by = NULL,
                    border_size = .1)

# p

png("plots/2-32-img2-voronoi-flat.png",
    width = 1700,
    height = 3000,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = bg_color))
p %>% print(vp = viewport())
dev.off()


# img 1 -------------------------------------------------------------------



png("plots/2-32-img1-voronoi.png",
    width = 3000,
    height = 1700,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = bg_color))
p %>% print(vp = viewport())
dev.off()



# img 3 -------------------------------------------------------------------

img <- load.image("~/Downloads/img3.jpeg")


p <- img_to_voronoi(img = img,
                    n_pix = 2500,
                    pick_by = value,
                    border_size = 0,
                    bg_color = "#6A5A77",
                    delaunay = T)

p

png("plots/2-32-img3-voronoi.png",
    width = 1700,
    height = 2400,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "#6A5A77"))
p %>% print(vp = viewport())
dev.off()


# img 4 -------------------------------------------------------------------

img <- load.image("~/Downloads/img4.jpeg")


p <- img_to_voronoi(img = img,
                    n_pix = 5000,
                    # pick_by = 1/(value + .0000001),
                    pick_by = (( c.1 * ( c.3 )^2)/(c.2 + 0.000001)),
                    border_size = .2,
                    bg_color = "#ffffff",
                    delaunay = F)

p

p <- img_to_voronoi(img = img,
                    n_pix = 1000,
                    # pick_by = 1/(value + .0000001),
                    pick_by = c.1,
                    border_size = .2,
                    bg_color = "#ffffff",
                    delaunay = F)

png("plots/2-32-img4-voronoi.png",
    width = 1700,
    height = 2400,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "#ffffff"))
p %>% print(vp = viewport())
dev.off()


# img 5 -------------------------------------------------------------------

img <- load.image("~/Downloads/img5.png")

p <- img_to_voronoi(img = img,
                    n_pix = 5000,
                    # pick_by = 1/(value + .0000001),
                    pick_by = c.1,
                    border_size = .2,
                    bg_color = "#ffffff",
                    delaunay = F,
                    delaunay_tile = F,
                    grays = F)

# p

png("plots/2-32-img5-voronoi.png",
    width = 1700,
    height = 2200,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "#CD0A39"))
p %>% print(vp = viewport())
dev.off()


p_del <- img_to_voronoi(img = img,
                    n_pix = 1000,
                    # pick_by = 1/(value + .0000001),
                    pick_by = c.1,
                    border_size = 1.8,
                    bg_color = "#ffffff",
                    delaunay = F,
                    delaunay_tile = T,
                    grays = F)

p_del


png("plots/2-32-img5-delaunay.png",
    width = 1700,
    height = 2200,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "#CD0A39"))
p_del %>% print(vp = viewport())
dev.off()

# ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#   ggforce::geom_delaunay_segment2(aes(colour = Species, group = -1), size = 2,
#                          lineend = 'round')
