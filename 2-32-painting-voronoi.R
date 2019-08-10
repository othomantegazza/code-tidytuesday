# Most steps are taken form:
# https://chichacha.netlify.com/2018/11/12/utilizing-k-means-to-extract-colours-from-your-favourite-images/


# set up ------------------------------------------------------------------


library(tidyverse)
library(imager)
library(ggvoronoi)
library(grid)

# background color
bg_color <- "#E8EDEF"

# Get image ---------------------------------------------------------------

image_path <- "data/2-32-bob-ross-sunset.Rdata"


if(!file.exists(image_path)) {
  img <- load.image("https://assets.atlasobscura.com/article_images/66876/image.jpg")
  
  save(img, file = image_path)
} else {
  load(image_path)
}


# analyze -----------------------------------------------------------------

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


# sample pixels ------------------------------------------------------------

set.seed(63); hex_pix_mini <- 
  hex_pix %>% 
  sample_n(2500, weight = value) # more likely if luminosity is higher 
  
# colors named vectors
# for plotting
pix_colors <- 
  hex_pix_mini %>% 
  pull(hexval) %>% 
  {purrr::set_names(x = .,
                   nm = .)}

# range of axis
range_x <- c(0, dim(img)[1])
range_y <-  c(dim(img)[2], 0)

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


# decorate plot with grid and save ----------------------------------------

# png parameters
img_height <- 2800
img_width <- 2300

# position of bottom left corner
img_x <- .2
img_y <- .18

# and plot size
plot_width <- 1 - img_x - .05
plot_height <- 1 - img_y - .05

# save
png(file = "plots/2-32-painting-voronoi.png",
    height = img_height,
    width = img_width,
    res = 300)
grid.newpage()
# background
grid.rect(gp = gpar(fill = "#838798"))
# plot
p %>% print(vp = viewport(x = img_x, y = img_y, 
                          just = c(0, 0),
                          height = plot_height,
                          width = plot_width))
# side caption
grid.text(label = str_wrap("Voronoi tesselation of one of Bob Ross paintigs. Inspired by @chisatini's blog.",
                           width = 14),
          x = img_x - .003, y = .945,
          hjust = 1, vjust = 1, gp = gpar(size = 14, lineheight = 1,
                                          col = bg_color))
# signature
grid.text(label = "Painting by Bob Ross | Plot by @othomn",
          x = .92, y = .1,
          hjust = 1, vjust = 1, gp = gpar(fontsize = 10, lineheight = 1,
                                          col = bg_color))
dev.off()


# save json for d3 --------------------------------------------------------

library(jsonlite)

hex_pix_mini %>% 
  toJSON() %>%
  {paste("var hexpix = ", .)} %>% 
  cat(file = "d3/json_data/2-32-painting-voronoi.js")
