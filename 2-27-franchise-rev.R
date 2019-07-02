library(tidyverse)
library(packcircles) ## easily package circles


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

packs <- 
  fr %>%
  group_by()
  arrange(desc(revenue)) %>% 
  pull(revenue) %>% 
  circleProgressiveLayout()


packs %>% 
  circleLayoutVertices(npoints = 200) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group=id)) +
  coord_equal() +
  theme_void()
 