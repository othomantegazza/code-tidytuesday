library(tidyverse)

dat_url <- paste0("https://raw.githubusercontent.com/",
              "rfordatascience/tidytuesday/master/data/",
              "2019/2019-04-16/Economist_women-research.csv")


dat_path <- "data/2-16-economist-post.Rdata"

if(!file.exists(dat_path)) {
  dat_raw <- readr::read_csv(dat_url)
  
  research_names <- c("country",
                      "Health sciences",
                      "Physical sciences",
                      "Engineering",
                      "Computer science, maths",
                      "Women inventores")
  
  dat <- 
    dat_raw %>% 
    na.omit() %>% 
    set_names(research_names) %>% 
    filter(country != "Country") %>% 
    gather(field, percent_women, `Health sciences`:`Women inventores`)
    
  
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


scale_custom <- scale_colour_viridis_c(limits = c(0,1),
                                       guide = FALSE)

# plot_circle <- function(percent_women)
dat %>% 
  mutate(percent_women = as.numeric(percent_women)) %>% 
  mutate(percent_men = 1 - percent_women) %>% 
  slice(1) %>% 
  gather(gender, value, percent_women:percent_men) %>% 
  ggplot() +
  geom_bar(aes(x = 1,
               y = value,
               fill = gender),
           stat = "identity") + 
  geom_text(data = . %>% 
              filter(gender == "percent_women"),
            aes(x = -.5, y = 0,
                label = value,
                colour = value),
            size = 10) +
  coord_polar(theta = "y") +
  lims(x = c(-.5, 1.5)) +
  scale_fill_viridis_d(begin = .1, end = .9, guide = FALSE) +
  scale_custom +
  theme_void()
