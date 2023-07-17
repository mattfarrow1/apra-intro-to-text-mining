# Required packages
# install.packages("tidyverse")
# install.packages("tidytext")
library(tidyverse)
library(tidytext)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download
df <- read_csv("DisneylandReviews.csv")

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
  head()

# Word count without stop words
tidy_sample %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word, sort = TRUE) %>% 
  head()

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
