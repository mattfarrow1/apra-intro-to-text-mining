
# Setup -------------------------------------------------------------------

# Required packages
# install.packages("tidyverse")
# install.packages("tidytext")
library(tidyverse)
library(tidytext)

# Optional packages
library(scales)
library(gt)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download
df <- read_csv("DisneylandReviews.csv")

# Take a look at the data
head(df)

# Look more closely at branch
head(df$Branch)

# Examine the data
str(df)
glimpse(df)

# Random sample reviews
df <- sample_n(df, 1000)

# Which parks do these reviews come from?
unique(df$Branch)

# Rename branches
df <- df %>% 
  rename(Park = Branch) %>% 
  mutate(Park = recode(Park,
                       "Disneyland_California" = "California",
                       "Disneyland_HongKong" = "Hong Kong",
                       "Disneyland_Paris" = "Paris"
  ))

# Now which parks do these reviews come from?
unique(df$Park)

# ggplot2 Intro via Exploratory Data Analysis -----------------------------

# Distribution of ratings
df %>% 
  ggplot(aes(Rating)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Distribution of ratings by park
df %>% 
  ggplot(aes(Rating, fill = Park)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       fill = "Park") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete() +
  theme_minimal()

# Distribution of ratings by park
df %>% 
  ggplot(aes(Rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~Park, ncol = 1, scales = "free_y") +
  theme_minimal()

df %>% 
  ggplot(aes(Park, Rating, color = Park)) +
  geom_boxplot(color = "black") +
  geom_jitter(alpha = 0.3) +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count") +
  theme_minimal()

# Examine Park Reviews ----------------------------------------------------

# Look at an example of a review
df$Review_Text[15]

# Convert it to a tibble
sample <- tibble(line = 1, text = df$Review_Text[15])
sample

# Unnest tokens
tidy_sample <- sample %>% 
  unnest_tokens(word, text)
tidy_sample

# Word count
tidy_sample %>% 
  count(word, sort = TRUE) %>% 
  head() %>% 
  gt()

# Word count without stop words
tidy_sample %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word, sort = TRUE) %>% 
  head() %>% 
  gt()

# Process all Reviews -----------------------------------------------------

# Number each review for each park
reviews <- df %>%
  group_by(Park) %>%
  mutate(linenumber = row_number()) %>% 
  ungroup() %>% 
  select(Park, linenumber, text = Review_Text) %>% 
  arrange(Park, linenumber)

# Unnest tokens and remove stop words
tidy_reviews <- reviews %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# Perform word count
tidy_reviews %>% 
  count(word, sort = TRUE) %>% 
  gt() %>% 
  tab_stubhead(label = "Park")

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

# Bigrams -----------------------------------------------------------------

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
  gt()

# Term Frequency ----------------------------------------------------------

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
  theme_ipsum()

# Custom Stop Words -------------------------------------------------------

my_stopwords <- tibble(word = c(as.character(1:10), 
                                "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                "v003", "v004", "v005", "v006", "v7"))
# nasa_title <- nasa_title %>% 
#   anti_join(my_stopwords)

# nasa_desc <- nasa_desc %>% 
#   anti_join(my_stopwords)
