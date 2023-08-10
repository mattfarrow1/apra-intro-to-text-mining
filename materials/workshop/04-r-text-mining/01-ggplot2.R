# Load libraries
library(tidyverse)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# Load data
load("materials/workshop_data.RData")

# Distribution of ratings
df %>% 
  ggplot(aes(Rating)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Distribution of ratings by park
df %>% 
  ggplot(aes(Rating, fill = Branch)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       fill = "Park") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_discrete() +
  theme_minimal()

# Distribution of ratings by park
df %>% 
  ggplot(aes(Rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Branch, ncol = 1, scales = "free_y") +
  theme_minimal()

df %>% 
  ggplot(aes(Branch, Rating, color = Branch)) +
  geom_boxplot(color = "black") +
  geom_jitter(alpha = 0.3) +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       color = "Park") +
  theme_minimal()
