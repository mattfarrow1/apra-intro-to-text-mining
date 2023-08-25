# When working with this script the first time, it's a good idea to start with a
# clean session. That will clear out your data and any saved items you've made.
# Go to [Session] -> [Restart R]

# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
# library(ggthemes)  # Not loaded, I call this explicitly in the code

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# Load data. This file is an R data-formatted file that contains a random sample
# of 1,000 reviews.
load("workshop_data.RData")

# Convert "Branch" to "Park" and clean up names. This is a repeat of what was
# done in the intro.R script.
df <- df |> 
  mutate(Branch = case_match(Branch, 
                             "Disneyland_California" ~ "California",
                             "Disneyland_HongKong" ~ "Hong Kong",
                             "Disneyland_Paris" ~ "Paris")) |> 
  rename("Park" = Branch)

# Visualize the Data ------------------------------------------------------

# Create a basic histogram of ratings
df |> 
  ggplot(aes(Rating)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count") +
  theme_minimal()

# Create a stacked bar chart of ratings by park. This is done by adding `fill`
# to the ggplot aesthetics.
df |> 
  ggplot(aes(Rating, fill = Park)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       fill = "Park") +
  scale_fill_brewer(palette = 3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Count the number of ratings per park per year
ratings_by_year <- df |> 
  mutate(Year = year(Year_Month),
         Month = month(Year_Month)) |> 
  group_by(Park) |> 
  count(Year) |> 
  rename(Ratings = n)

ratings_by_year

# If your goal was to make a table, and not a chart, you can add the
# `pivot_wider` function to your code.
ratings_by_year |> 
  pivot_wider(names_from = Park,
              values_from = Ratings) |> 
  arrange(desc(Year))

# Reviews by year (line plot)
ratings_by_year |> 
  ggplot(aes(Year, Ratings, color = Park)) +
  geom_line() +
  labs(title = "Ratings by Park per Year",
       x = "Year",
       y = "Ratings") +
  scale_x_continuous(breaks = c(2010,
                                2011,
                                2012,
                                2013,
                                2014,
                                2015,
                                2016,
                                2017,
                                2018,
                                2019,
                                2020)) +
  ggthemes::theme_economist()

# Sample the Data ---------------------------------------------------------

# Brackets allow us to isolate a single cell.
df$Review_Text[11]

# Convert the data into a tibble
sample <- tibble(
  line = 1,
  text = df$Review_Text[11]
)

# Unnest tokens
sample |> 
  unnest_tokens(word, text)

# Process the Data --------------------------------------------------------

# Number each review for each park
reviews <- df |> 
  group_by(Park)  |> 
  mutate(linenumber = row_number()) |> 
  ungroup() |>  # Useful to make sure grouping doesn't impact you later
  select(Park, linenumber, text = Review_Text) |> 
  arrange(Park, linenumber)

# Unnest tokens
tidy_reviews <- reviews |> 
  unnest_tokens(word, text)

# tidy_reviews at this point should have 124732 observations and 3 variables

# Remove stop words
tidy_reviews <- tidy_reviews |> 
  anti_join(stop_words)

# tidy_reviews at this point should have 45011 observations and 3 variables

# Count the most frequent words
tidy_reviews |> 
  count(word, sort = TRUE)

# Word Clouds -------------------------------------------------------------

# Create a word cloud
tidy_reviews |> 
  count(word) |> 
  with(wordcloud(word, 
                 n,
                 max.words = 40, 
                 colors = brewer.pal(8, "Dark2")))

# There are a number of words that appear frequently and overwhelm potentially
# interesting data. We can create a custom list of words to exclude.
wc_removals <- c("day", "disney", "disneyland", "rides", "park")

# Create a word cloud, removing our custom words too
tidy_reviews |> 
  filter(!word %in% wc_removals) |> 
  count(word) |> 
  with(wordcloud(word, 
                 n,
                 max.words = 40, 
                 colors = brewer.pal(8, "Dark2")))

# N-Grams -----------------------------------------------------------------

# Unnest the original reviews into bigrams
tidy_bigrams <- reviews |> 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |> 
  filter(!is.na(bigram))

# tidy_bigrams should have 123732 observations now
tidy_bigrams

# Separate each word
tidy_bigrams <- tidy_bigrams |> 
  separate(bigram, c("word1", "word2"), sep = " ")

tidy_bigrams

# Remove the stop words
tidy_bigrams <- tidy_bigrams |> 
  filter(!word1 %in% stop_words$word) |> 
  filter(!word2 %in% stop_words$word)

# tidy_bigrams should have 13844 observations now
tidy_bigrams

# What are the most frequent bigrams
tidy_bigrams %>% 
  count(word1, word2, sort = TRUE)

# Reunite the two words
tidy_bigrams <- tidy_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

tidy_bigrams

# Look at the top bigrams by park
tidy_bigrams |> 
  group_by(Park) |> 
  count(bigram) |> 
  arrange(desc(n))

# Word Frequencies --------------------------------------------------------

# Calculate the most common words by Park
review_words <- reviews |> 
  unnest_tokens(word, text) |> 
  count(Park, word, sort = TRUE)

review_words

# Calculate the total number of words per park
total_words <- review_words |> 
  group_by(Park) |> 
  summarize(Total = sum(n))

total_words

# Join the data together
review_words <- left_join(review_words, total_words)

review_words

# Calculate the term frequency
freq_by_rank <- review_words |> 
  group_by(Park) |> 
  mutate(rank = row_number(),
         freq = n/Total) |> 
  ungroup()

freq_by_rank

# Calculate TF-IDF
review_tf_idf <- review_words |> 
  bind_tf_idf(word, Park, n)

review_tf_idf

# Look at reviews with a high tf-idf
review_tf_idf |> 
  select(-Total) |> 
  arrange(desc(tf_idf))

# Visualize high tf-idf words
review_tf_idf |> 
  group_by(Park) |> 
  slice_max(tf_idf, n = 10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Park)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Park, ncol = 1, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Sentiment Analysis ------------------------------------------------------

# Look at AFINN
get_sentiments(lexicon = "afinn") |> 
  sample_n(10)

# AFINN example
tidy_reviews |> 
  group_by(Park, linenumber) |> 
  inner_join(get_sentiments("afinn")) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  ggplot(aes(linenumber, value, fill = Park)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~Park, ncol = 1, scales = "free_x")

# Look at bing
get_sentiments(lexicon = "bing") |> 
  sample_n(10)

# bing example
tidy_reviews |> 
  group_by(Park) |> 
  inner_join(get_sentiments("bing")) |> 
  count(Park, index = linenumber, sentiment) |> 
  ungroup() |> 
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) |> 
  mutate(sentiment = positive - negative) |> 
  ggplot(aes(index, sentiment, fill = Park)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~Park, ncol = 1, scales = "free_x")

# Look at nrc
get_sentiments(lexicon = "nrc") |> 
  sample_n(10)

# nrc example
tidy_reviews |> 
  right_join(get_sentiments("nrc"),
             relationship = "many-to-many") |> 
  filter(!is.na(sentiment)) |> 
  count(sentiment, sort = TRUE)
