library(tidyverse)
library(glue)

# Get Data ----------------------------------------------------------------


dat_path <- "data/34-thanksgiving.Rdata"


if(!file.exists(dat_path)) {
  dat <- 
    read_csv(paste0("https://raw.githubusercontent.com/",
                    "rfordatascience/tidytuesday/master/data/",
                    "2018-11-20/thanksgiving_meals.csv"))
  
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


# Alluvial plot -------------------------------------------------------------

dat_smp <- 
  dat %>% 
  filter(celebrate == "Yes") %>%
  select(
    age,
    # gender,
    # prayer,
    friendsgiving,
    # black_friday,
    # community_type,
    # us_region,
    cranberry
    ) %>%
  filter(complete.cases(.)) %>%
  filter(!cranberry %>% str_detect("Other")) %>%
  group_by_all() %>% 
  count()
  

library(ggalluvial)
p_all <- 
  dat_smp %>% 
  ggplot(aes(
    axis1 = cranberry,
    axis2 = friendsgiving,
    axis3 = age,
    y = n)) +
  theme_minimal() +
  geom_alluvium(aes(fill = cranberry)) +
  geom_stratum(fill = "grey98") +
  geom_text(stat = "stratum",
            label.strata = TRUE) +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Cranberry Sauce",
                                "Friendsgiving",
                                "Age")) +
  scale_fill_viridis_d() +
  labs(title = "Cranberry Sauce Types at Thanksgiving",
       subtitle = "Data Polled on Nov. 17, 2015",
       fill = "Cranberry\nSauce",
       caption = "Data source: fivethirtyeight.com | Plot by @othomn")


png(filename = "plots/34-thanksgiving_all.png",
    height = 1400, width = 2300,
    res = 300)
p_all %>% print()
dev.off() 


  
# bar plot ----------------------------------------------------------------

dat_ratio <- 
  dat %>% 
  select(cranberry,
         us_region) %>%
  filter(complete.cases(.)) %>%
  filter(!cranberry %>% str_detect("Other")) %>% 
  group_by_all() %>% 
  count() %>% 
  group_by(us_region) %>%
  mutate(all = n %>% sum()) %>% 
  ungroup() %>% 
  mutate(ratio = n/all) 
  
lvl <- dat_ratio %>%
  filter(cranberry == "Homemade") %>%
  arrange(ratio) %>%
  pull(us_region)
  
p_bar <- 
  dat_ratio %>% 
  mutate(us_region = factor(us_region,
                            levels = lvl),
         cranberry = factor(cranberry,
                            levels = (c(
                              "None", "Canned", "Homemade"
                              )))) %>% 
  ggplot(aes(x = us_region,
             y = ratio,
             fill = cranberry)) +
  geom_bar(stat = "identity",
           colour = "grey80") +
  geom_text(data = . %>%
              group_by(us_region) %>%
              summarise(n = sum(n)),
            aes(label = glue("n = {n}"),
                fill = NULL),
            y = 0.02, 
            colour = "#208C88", 
            hjust = 0) +
  scale_fill_viridis_d(option = "D") +
  coord_flip(expand = FALSE) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "grey40"), 
        title = element_text(colour = "grey20")) +
  labs(x = "",
       title = "Where is Homemade Cranberry Sauce Most Common?",
       subtitle = "For Thanksgiving, Data Polled on Nov. 17, 2015",
       fill = "Cranberry\nSauce",
       caption = "Data source: fivethirtyeight.com | Plot by @othomn")

png(filename = "plots/34-thanksgiving.png",
    height = 1400, width = 2300,
    res = 300)
p_bar %>% print()
dev.off() 

