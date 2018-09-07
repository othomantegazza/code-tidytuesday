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

dat %>%
  filter(vit_a > 150 | vit_c > 150) %>%
  View()

tidy_fit <- dat %>%
  lm(formula = calories ~ total_fat,
     data = .) %>%
  tidy()

fit %>% 
  augment() %>%
  View()

augment(tidy_fit)

get_kword_cals <- function(kword)
{
  print(kword)
  dat %>%
    filter(str_detect(item,
                      fixed(kword))) %>%
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
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

"https://sharlagelfand.netlify.com/posts/tidy-ttc/"  
"http://varianceexplained.org/r/op-ed-text-analysis/?utm_content=bufferb84e1&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer"   