library(tidyverse)
library(countrycode)
library(scales)

# want to plot cost/GDP for countries split by continents


# can I find the continents in the package countrycode?
# str(countrycode::codelist)
# str(countrycode::cldr_examples)
# str(countrycode::codelist_panel)
# yes


# get the data for cost/GDP -----------------------------------------------

gdp_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                  "tidytuesday/master/data/2018-09-25/table_3.csv")

gdp_file <- "data/26-invasive-gdp-ratio.Rdata"

if(!file.exists(gdp_file)) {
  dat_gdp <- read_csv(gdp_url)
  save(dat_gdp, file = gdp_file)
} else {
  load(gdp_file)
}


# get data for risk -------------------------------------------------------

risk_url <- paste0("https://raw.githubusercontent.com/rfordatascience/",
                   "tidytuesday/master/data/2018-09-25/table_1.csv")

risk_file <- "data/26-risk.Rdata"

if(!file.exists(risk_file)) {
  dat_risk <- read_csv(risk_url)
  save(dat_risk, file = risk_file)
} else{
  load(risk_file)
}



# fix country names of gdp dataset ----------------------------------------

# this is error prone

dat_gdp <- dat_gdp %>%
  filter(! country %in% c("Guinea", "Niger")) %>%
  mutate(country = pmap(., ~grep(paste0("^", ..1),
                x = dat_risk$country,
                value = TRUE)) %>%
           unlist())

# Full data ---------------------------------------------------------------

dat <- dat_risk %>%
  select(-rank) %>%
  full_join(dat_gdp) %>%
  drop_na(invasion_cost) %>%
  left_join(codelist_panel %>%
              rename(country = country.name.en) %>%
              select(country, continent, region) %>%
              distinct()) %>%
  # fix missing continents
  mutate(continent = replace(continent, country == "Czech Republic", "Europe"),
         continent = replace(continent, country == "Bosnia and Herzegovina", "Europe"),
         continent = replace(continent, country == "USA", "Americas"),
         continent = replace(continent, country == "Korea Republic of", "Asia"),
         continent = replace(continent, country == "Georgia (Republic)", "Europe"),
         continent = replace(continent, country == "Congo (Republic of)", "Africa"),
         continent = replace(continent, country == "Trinidad and Tobago", "Americas"),
         continent = replace(continent, country == "Russian Federation", "Asia")
  )

dat %>% filter(is.na(continent)) %>% View

# plot --------------------------------------------------------------------

png(filename = "plots/26-invasives.png",
    height = 4700, width = 1500,
    res = 300)
dat %>%
  ggplot(aes(x = reorder(country,
                         gdp_proportion),
             y = gdp_proportion,
             size = invasion_threat,
             colour = log(invasion_cost))) +
  geom_point(alpha = .8) +
  facet_grid(continent ~ .,
             scales = 'free',
             space = "free") +
  # facet_wrap(facets = "continent", ncol = 1, scales = "free_y") +
  coord_flip() +
  scale_size_continuous(range = c(.1,3)) +
  scale_color_viridis_c() +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  labs(title = "Agricolture is Affected by\nInvasive Species",
       x = "",
       y = "Cost in Proportion to GDP",
       caption = "data: griis.org; Paini et al., PNAS July 5, 2016 \n plot by @othomn",
       size = "Invasion Threat",
       colour = "Invasion Costs [log USD]")
dev.off()  

# Scatterplot -------------------------------------------------------------


png(filename = "plots/26-invasives-xy.png",
    height = 2200, width = 2200,
    res = 300)
dat %>%
  ggplot(aes(x = invasion_cost,
             y = gdp_proportion,
             colour = continent,
             size = invasion_threat)) +
  geom_point(alpha = .7) +
  scale_x_log10() +
  scale_y_continuous(trans = "logit",
                     breaks = boot::inv.logit(c(-10, -8, -6,-4, -2, 0, 2, 4)),
                     labels = scales::percent) +
  scale_size_continuous(range = c(.1,4)) +
  theme_minimal()
dev.off()
