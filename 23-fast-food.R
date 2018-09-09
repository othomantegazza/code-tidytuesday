library(tidyverse)
library(ggrepel)
library(broom)

# Get data ----------------------------------------------------------------

dat_path <- "data/23-fast-food.Rdata"


if(!file.exists(dat_path)) {
  dat <- read_csv(paste0("https://raw.githubusercontent.com/",
                         "rfordatascience/tidytuesday/master/",
                         "data/2018-09-04/fastfood_calories.csv")) %>%
    select(-X1)
  save(dat, file = dat_path)
} else {
  load(dat_path)
}


# Explore data ------------------------------------------------------------

dat %>%
  select_if(is.numeric) %>%
  pairs()

ggplot(dat,
       aes(x = vit_c,
           y = vit_a)) +
  geom_point(alpha = .1) +
  geom_text_repel(data = dat %>%
                    filter(vit_a > 150 | vit_c > 150),
                  aes(x = fiber,
                      y = vit_a,
                      label = item))

# dat %>%
#   filter(vit_a > 150 | vit_c > 150) %>%
#   View()


# Try Broom ---------------------------------------------------------------



fit <- dat %>%
  lm(formula = calories ~ total_fat,
     data = .)

tidy_fit <- fit %>% tidy()

fit %>% 
  augment() %>%
  View()

augment(tidy_fit)


# Check words in item's name ----------------------------------------------


get_kword_cals <- function(kword)
{
  print(kword)
  dat %>%
    filter(str_detect(item,
                      coll(kword))) %>%
    mutate(keyword = kword) #%>%
    # select(keyword, calories)
}

dat_kword <- str_split(dat$item, " ") %>%
  unlist() %>%
  unique() %>%
  # str_remove("\(") %>%
  map(get_kword_cals) %>%
  purrr::reduce(bind_rows)
  

tst <- dat_kword %>%
  group_by(keyword) %>%
  add_tally() %>%
  filter(n > 10)

tst %>% summarise(cal_median = median(calories),
            n = n()) %>%
  arrange(desc(cal_median))

tst %>% ggplot(aes(x = reorder(keyword, calories),
                   y = calories)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5,
                                   hjust = 1)) +
  coord_flip()
  
dat %>% 
  filter(grepl("King", item)) %>%
  View()

# Check PCA ---------------------------------------------------------------

dat_pc <- dat %>% 
  replace_na(replace = list(vit_a = 0,
                            vit_c = 0,
                            calcium = 0)) %>%
  # column_to_rownames("restaurant") %>%
  drop_na()

pc <- dat_pc %>% 
  select_if(is.numeric) %>%
  prcomp()

pc %>%
  tidy()

pc_tidy <- pc %>% 
  augment(., data = dat_pc)

pc_tidy %>%
  ggplot(aes(x = restaurant,
             y = .fittedPC9)) +
  geom_jitter(height = 0) 

dat %>%
  arrange(desc(sodium)) %>%
  View()

"https://sharlagelfand.netlify.com/posts/tidy-ttc/"  
paste0("http://varianceexplained.org/r/op-ed-text-analysis/",
       "?utm_content=bufferb84e1&utm_medium=social&utm_source=twitter.",
       "com&utm_campaign=buffer")