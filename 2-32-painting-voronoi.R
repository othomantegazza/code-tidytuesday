# Most steps are taken form:
# https://chichacha.netlify.com/2018/11/12/utilizing-k-means-to-extract-colours-from-your-favourite-images/

library(tidyverse)
library(imager)
library(ggvoronoi)
library(grid)
bg_color <- "#E8EDEF"


image_path <- "data/2-32-bob-ross-sunset.Rdata"


if(!file.exists(image_path)) {
  img <- load.image("https://assets.atlasobscura.com/article_images/66876/image.jpg")
  
  save(img, file = image_path)
} else {
  load(image_path)
}

# plot(img)

# number of pixel?
dim(img)[1]*dim(img)[2]

# colours: hex value for every pixel
hex_pix <- 
  img  %>% 
  as.data.frame(wide = "c") %>% 
  mutate(hexval = rgb(c.1,c.2,c.3))

# luminosity for every pixes
grey_pix <- 
  img %>% 
  grayscale() %>% 
  as.data.frame()

# merge
hex_pix <- 
  hex_pix %>%
  inner_join(grey_pix)

# p <- ggplot(hex_pix,aes(x,y))+geom_raster(aes(fill=hexval))+scale_fill_identity()
# p

set.seed(63); hex_pix_mini <- 
  hex_pix %>% 
  sample_n(2500, weight = value) 
  
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
  ggvoronoi::geom_voronoi(aes(fill = hexval),
                          colour = bg_color,
                          size = .2) +
  scale_y_reverse(limits = range_y,
                  expand = expand_scale(mult = .01)) +
  scale_x_continuous(limits = range_x,
                     expand = expand_scale(mult = .01)) +
  scale_fill_manual(values = pix_colors, guide = FALSE) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color),
        plot.margin = margin(0,0,0,0))

# svglite::svglite(file = "plots/2-32-painting-voronoi.svg")
# p %>% print()
# dev.off()

# png parameters
img_height <- 2800
img_width <- 2300

# position of top left corner
img_x <- .2
img_y <- .18
plot_width <- 1 - img_x - .05
plot_height <- 1 - img_y - .05

png(file = "plots/2-32-painting-voronoi.png",
    height = img_height,
    width = img_width,
    res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "#838798"))
p %>% print(vp = viewport(x = img_x, y = img_y, 
                          just = c(0, 0),
                          height = plot_height,
                          width = plot_width))
grid.text(label = str_wrap("Voronoi tesselation of one of Bob Ross paintigs. Inspired by @chisatini's blog.",
                           width = 14),
          x = img_x - .003, y = .945,
          hjust = 1, vjust = 1, gp = gpar(size = 14, lineheight = 1,
                                          col = bg_color))
grid.text(label = "Painting by Bob Ross | Plot by @othomn",
          x = .92, y = .1,
          hjust = 1, vjust = 1, gp = gpar(fontsize = 10, lineheight = 1,
                                          col = bg_color))
dev.off()

