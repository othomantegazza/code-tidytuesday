library(tidyverse)
library(lubridate)
library(countrycode)
library(ggforce)
library(ggrepel)

# Get data ----------------------------------------------------------------

managed_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                   "master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

mismanaged_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
                         "master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")


data_path <- "data/2-21-plastic-waste.Rdata"

if(!file.exists(data_path)) {
  waste_ok <- read_csv(managed_url) %>% 
    janitor::clean_names() %>% 
    filter(year == 2010)
  
  waste_lost <- read_csv(mismanaged_url) %>% 
    janitor::clean_names() %>% 
    filter(year == 2010)
  
  save(waste_ok, waste_lost, file = data_path)
} else {
  load(data_path)
}


waste <-
  full_join(waste_ok %>%
              select(entity,
                     code, 
                     gdp_per_capita = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
                     plastic = per_capita_plastic_waste_kilograms_per_person_per_day),
            waste_lost %>% 
              select(entity,
                     code,
                     lost = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day)) %>% 
  # select(entity, code, year,
  #        ok = per_capita_plastic_waste_kilograms_per_person_per_day,
  #        lost = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day) %>% 
  drop_na() 

# Explore -----------------------------------------------------------------

waste %>% 
  ggplot(aes(x = plastic,
             y = lost)) +
  geom_point() 
  # scale_x_log10() +
  # scale_y_log10()
  # ggrepel::geom_text_repel(aes(label = code))

waste <- 
  waste %>% 
  mutate(ratio_lost = lost/plastic)

# two groups?
waste %>% 
  ggplot(aes(x = ratio_lost)) +
  geom_histogram()

waste %>%
  filter(ratio_lost > .75) %>% 
  pull(entity)

waste %>%
  filter(ratio_lost <.1 ) %>% 
  pull(entity)


# with continent ----------------------------------------------------------

waste_cont <- 
  waste %>% mutate(continent = countrycode(sourcevar = entity,
                                         origin = "country.name",
                                         destination = "continent"),
                 continent = case_when(entity == "Micronesia (country)" ~ "Oceania",
                                    TRUE ~ continent)) %>% 
  drop_na(continent)

p <- 
  waste_cont %>% 
  ggplot(aes(x = ratio_lost,
             fill = continent,
             colour = continent)) +
  geom_histogram(bins = 25, alpha = .5) +
  # geom_mark_rect(data = tibble(x = .07,
  #                              y = 40,
  #                              label = "Ciao",
  #                              description = "Ciao ciao ciao"),
  #                aes(x = x,
  #                    y = y,
  #                    label = label,
  #                    description = description),
  #                colour = "white", position = "right",
  #                inherit.aes = F) +
  annotate(geom = "text", x = .12, y = 41, 
                label = str_wrap("High income countries can invest in
                                 plastic waste manegement?",
                                 width = 20),
           hjust = 0, vjust = 0,
           size = 3, lineheight = 1,
           color = "grey10") +
  annotate(geom = "segment", x = .12,
           y = 40, yend = 37, xend = .09,
           color = "grey10") +
  annotate(geom = "text", x = .76, y = 18, 
           label = str_wrap("Low income countries?",
                            width = 20),
           hjust = 1, vjust = 0,
           size = 3, lineheight = 1,
           color = "grey10") +
  annotate(geom = "segment", x = .76,
           y = 17.5, yend = 16, xend = .78,
           color = "grey10") +
  geom_hline(yintercept = 0, colour = "grey50") +
  theme_minimal() +
  theme(text = element_text(family = "sans",
                            colour = "grey20"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = "[Mismanaged/Total] waste in Kg per capita",
       y = "Number of Countries",
       fill = "Continent", colour = "Continent",
       title = "Does Plastic Waste Management Mirrors Inequality?",
       subtitle = "Ratio of mismanaged plastic waste per country in 2010.",
       caption = "Source: Our World in Data | Plot by @othomn")

png("plots/2-21-plastic-waste.png",
    res = 300,
    height = 1400,
    width = 2200)
p %>% print()
dev.off()

# My Wednesday #TidyTuesday #rstats

# Is inequality mirrored also in waste plastic management?

# I checked the distribution of the percentage of mismanaged plastic waste, and it's kind of bimodal.

# https://ourworldindata.org/plastic-pollution
# https://github.com/othomantegazza/code-tidytuesday/blob/master/2-21-plastic-waste.R


# scatterplot -------------------------------------------------------------

annos_df <- 
  tibble(x = c(),
         y = c)

# p_s <- 
  waste_cont %>% 
  ggplot(aes(x = plastic,
             y = lost,
             size = gdp_per_capita,
             colour = continent)) +
  geom_point(alpha  = .7) +
  geom_text_repel(data = tibble(x = .05,
                                y = .005,
                                label= str_wrap("High income countries can invest in
                                plastic waste manegement?",
                                                width = 20)),
                  aes(x = x, y = y, label = label),
                  inherit.aes = F,
                  hjust = 1, vjust = 0,
                  size = 3, lineheight = 1,
                  color = "grey10") +
  # annotate(geom = "text", x = 1.3, y = 0.014, 
  #          label = str_wrap("High income countries can invest in
  #                                plastic waste manegement?",
  #                           width = 20),
           # hjust = 0, vjust = .5,
           # size = 3, lineheight = 1,
           # color = "grey10") +
  # annotate(geom = "segment",
  #          arrow = arrow(length = unit(1, "mm"), type = "closed"), 
  #          x = 1.3, y = .014,
  #          xend = 1, yend = .014) +
  scale_x_log10(limits = c(.001, 10)) +
  scale_y_log10(limits = c(.0005, .3)) +
  theme_minimal() 


png("plots/2-21-plastic-waste-scatterplot.png",
    res = 300,
    height = 1400,
    width = 2200)
p_s %>% print()
dev.off()


# cluster -----------------------------------------------------------------

set.seed(49)

waste_clust <- 
  waste_cont %>% 
  transmute(plastic = log(plastic),
            lost = log(lost),
            gdp_per_capita = log(gdp_per_capita)) %>% 
  kmeans(centers = 3) %>% 
  {bind_cols(waste_cont, tibble(cluster = .$cluster))}


cl_1 <- "Medium GDP, high plastic loss"
cl_2 <- "High GDP, low plastic loss"
cl_3 <- "Low GDP, medium plastic loss"


p_clust <- 
waste_clust %>% 
  mutate(label = case_when(cluster == 1 ~ cl_1,
                                 cluster == 2 ~ cl_2,
                                 cluster == 3 ~ cl_3)) %>%
  # mutate(plastic = log(plastic),
  #           lost = log(lost)) %>% 
  ggplot(aes(x = plastic,
             y = lost,
             colour = cluster)) +
  geom_point(aes(size = gdp_per_capita)) +
  geom_mark_ellipse(aes(group = cluster,
                        label = cluster,
                        description = label),
                    # label.margin = .2,
                    size = 1.1, con.size = 1.3,
                    label.fontsize = 10, label.fill = "grey90",
                    label.minwidth = 30) +
  scale_x_log10(
    limits = c(.01, 5)
    ) +
  scale_y_log10(
    limits = c(.0008, .8)
    ) +
  theme_minimal() +
  scale_colour_continuous(guide = FALSE) +
  labs(x = "Per capita plastic waste (kg per person per day)",
       y = "Per capita mismanaged plastic waste (kg per person per day)",
       size = "GDP per capita",
       caption = "Source: Our World in Data | Plot by @othomn") +
  theme(axis.title = element_text(size = 12))


png("plots/2-21-plastic-waste-cluster.png",
    res = 300,
    height = 2300,
    width = 3000)
p_clust %>% print()
dev.off()

waste_clust %>% 
  mutate(label = case_when(cluster == 1 ~ cl_1,
                           cluster == 2 ~ cl_2,
                           cluster == 3 ~ cl_3)) %>%
  # mutate(plastic = log(plastic),
  #           lost = log(lost)) %>% 
  ggplot(aes(x = gdp_per_capita,
             y = lost,
             colour = cluster)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

waste_clust %>% 
  mutate(label = case_when(cluster == 1 ~ cl_1,
                           cluster == 2 ~ cl_2,
                           cluster == 3 ~ cl_3)) %>%
  # mutate(plastic = log(plastic),
  #           lost = log(lost)) %>% 
  ggplot(aes(x = gdp_per_capita,
             y = plastic,
             colour = cluster)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

