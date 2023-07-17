# Intro to Sentiment Analysis ---------------------------------------------

# Get 'joy' sentiment
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# Most common 'joy' words in the reviews
tidy_reviews %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Sentiment Analysis by Park ----------------------------------------------

tidy_reviews_sentiment <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(Park, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(tidy_reviews_sentiment, aes(index, sentiment, fill = Park)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Park, ncol = 1, scales = "free_x") +
  labs(title = "Sentiment Analysis by Park") +
  theme_minimal()