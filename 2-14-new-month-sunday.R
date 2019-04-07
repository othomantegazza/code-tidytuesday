looper

is_new_month <- rollify(.f = ~month(.) %>% unique() %>% length(),
                        window = 8)

to_plot %>% pull(time_stamp) %>% 
  unique() %>%
  # Each day
  {tibble(time_stamp = round_date(., unit = "day"))} %>% 
  distinct() %>% 
  mutate(is_sunday = wday(time_stamp) == 1,
         new_month = is_new_month(time_stamp) == 2,
         new_month = case_when(is.na(new_month) ~ TRUE,
                               TRUE ~ new_month)) %>% 
  filter(is_sunday) %>% View()
