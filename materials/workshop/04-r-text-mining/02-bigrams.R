# Unnest into bigrams
tidy_bigrams <- reviews %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram))

# Separate words
tidy_bigrams <- tidy_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove stop words
tidy_bigrams <- tidy_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts:
tidy_bigrams %>% 
  count(word1, word2, sort = TRUE)

# Reunite terms
tidy_bigrams <- tidy_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

tidy_bigrams %>% 
  group_by(Park) %>% 
  count(bigram) %>% 
  arrange(desc(n)) %>% 
  head()