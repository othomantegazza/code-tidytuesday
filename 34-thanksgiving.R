library(tidyverse)
library(data.tree)
library(igraph)

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


# Explore -----------------------------------------------------------------

dat %>% 
  group_by(celebrate) %>%
  tally() 

dat_smp <- 
  dat %>% 
  filter(celebrate == "Yes") %>% 
  select(main_dish,
         main_prep,
         stuffing, cranberry) %>% 
  # mutate_all(as_factor)
  group_by_all() %>%
  count()

library(ggalluvial)
dat_smp %>% 
  ggplot(aes(axis1 = main_dish, axis2 = main_prep, axis3 = stuffing,
             y = n)) +
  geom_alluvium(aes(fill = cranberry)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE)


# dat_smp %>% treemap::treemap(index = c("main_dish",
#                                        "main_prep",
#                                        "stuffing"),
#                              vSize = "n")
    
tst <- dat_smp %>% 
  mutate(pathString = paste("Dinner", 
                            main_dish,
                            stuffing,
                            cranberry,
                            sep = "/")) %>%
  data.tree::as.Node()

tst %>% print("n")

tst %>% data.tree::as.igraph.Node()

