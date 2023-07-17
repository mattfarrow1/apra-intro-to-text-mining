park_words <- tidy_reviews %>%
  count(park, word, sort = TRUE)

total_words <- park_words %>% 
  group_by(park) %>% 
  summarize(total = sum(n))

park_words <- left_join(park_words, total_words)

freq_by_rank <- park_words %>% 
  group_by(park) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

park_tf_idf <- park_words %>%
  bind_tf_idf(word, park, n)

park_tf_idf

park_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

park_tf_idf %>%
  group_by(park) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = park)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~park, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_minimal()