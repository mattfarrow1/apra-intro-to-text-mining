# Load libraries
library(tidyverse)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# For the purposes of this workshop, we are using a sample of 1,000 reviews from
# the original data. This is to conserve memory and bandwidth, and to ensure
# everyone is looking at the same data. After the workshop, ff you would like to
# open the full data, it has been included in this repository.
# df <- read_csv("DisneylandReviews.csv")
load("workshop_data.RData")

# Look at the structure of the data
str(df)

# Using dplyr's `glimpse` function
glimpse(df)

# Look at the first n rows
head(df)

# Look at the last n rows
tail(df)

# Look at where the reviewers are from
unique(df$Reviewer_Location)

# What parks have been reviewed
unique(df$Branch)

# Let's drop "Disneyland_" from the start of the park names for clarity and
# rename that column to "Park"
df <- df %>% 
  mutate(Branch = case_match(Branch, 
                             "Disneyland_California" ~ "California",
                             "Disneyland_HongKong" ~ "Hong Kong",
                             "Disneyland_Paris" ~ "Paris")) |> 
  rename("Park" = Branch)

# Look at the final data set
glimpse(df)
