library(tidyverse)
library(ggforce)
# library(emojifont)
library(showtext)

font_families()

font_add(family = 'FontAwesome', regular = 'data/Font Awesome 5 Free-Solid-900.otf')



# get data ----------------------------------------------------------------


data_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-06-18/bird_counts.csv")

data_path <- "data/2-25-birds.Rdata"

if(!file.exists(data_path)) {
  birds <- 
    data_url %>% 
    read_csv()
  
  save(birds, file = data_path)
} else {
  load(data_path)
}

# explore -----------------------------------------------------------------

birds %>% 
  count(year) %>% View()

birds %>% 
  count(species) %>% View()

birds %>% summary

birds %>% 
  ggplot(aes(x = total_hours)) +
  geom_histogram()


# Plot --------------------------------------------------------------------

med_20 <- 
  birds %>% 
  filter(year > 1997,
         year <= 2007) %>% 
  drop_na() %>% # no need
  group_by(species) %>% 
  summarise(count_20 = median(how_many_counted_by_hour))


med_10 <- 
  birds %>% 
  filter(year > 2007,
         year <= 2017) %>% 
  drop_na() %>% # no need
  group_by(species) %>% 
  summarise(count_10 = median(how_many_counted_by_hour))

to_plot <- 
  full_join(med_20, med_10) %>% 
  filter(!{count_20 < .007 & count_10 < .007})

bird_img <- paste0("https://upload.wikimedia.org/wikipedia/commons/",
                   "thumb/c/c7/Font_Awesome_5_solid_dove.svg/",
                   "512px-Font_Awesome_5_solid_dove.svg.png")

bird_fa <- "plots/dove-solid.svg"

showtext_auto()
font_families()

p <- 
  to_plot %>% 
  ggplot(aes(x = reorder(species, count_20), 
             y = count_10)) +
  # ggimage::geom_image(image = bird_fa, size = .015) +
  # geom_text(label = "dove")
  geom_link(aes(xend = reorder(species, count_10),
                y = count_20,
                yend = count_10,
                size = ..index..,
                alpha = ..index..)) +
  # geom_point(colour = "red") +
  # geom_text(label = fontawesome("fa-github"), family = "fontawesome-webfont") +
  geom_text(label = -as.hexmode("f4ba"), family = "FontAwesome") +
  geom_fontawesome() +
  coord_flip() +
  scale_y_log10() +
  scale_size_continuous(range = c(.1, 1.5)) +
  guides(size = FALSE,
         alpha = FALSE)

p

png(filename = "plots/2-25-birds.png",
    res = 300,
    height = 3000,
    width = 1000)
p %>% print()
dev.off()

svg(filename = "plots/2-25-birds.svg",
    width = 6)
p %>% print()
dev.off()


birds %>% 
  filter(species %>% str_detect("winged Scoter")) %>% View()

birds %>% 
  filter(species %>% str_detect("Northern Pintail")) %>% View()
