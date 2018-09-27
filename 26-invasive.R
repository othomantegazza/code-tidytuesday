library(tidyverse)
library(countrycode)

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
              distinct()) 


# plot --------------------------------------------------------------------

png(filename = "plots/26-invasives.png",
    height = 4000, width = 1700,
    res = 300)
dat %>%
  ggplot(aes(x = reorder(country,
                         gdp_proportion),
             y = gdp_proportion,
             size = invasion_threat,
             colour = log(invasion_cost))) +
  geom_point(alpha = .8) +
  # facet_grid(continent ~ .,
  #            scales = 'free',
  #            space = "free") +
  facet_wrap(facets = "continent", ncol = 1, scales = "free_y") +
  coord_flip() +
  scale_size_continuous(range = c(.1,3)) +
  scale_color_viridis_c() +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
dev.off()  
