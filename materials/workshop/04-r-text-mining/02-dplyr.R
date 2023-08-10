
# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# Load data
df <- read_csv("DisneylandReviews.csv")

# Intro to dplyr
# https://dplyr.tidyverse.org

# mutate() adds new variables that are functions of existing variables
# select() picks variables based on their names.
# filter() picks cases based on their values.
# summarise() reduces multiple values down to a single summary.
# arrange() changes the ordering of the rows.

# Try mutate
df <- df %>% 
  mutate(Branch = case_match(Branch, 
                             "Disneyland_California" ~ "California",
                             "Disneyland_HongKong" ~ "Hong Kong",
                             "Disneyland_Paris" ~ "Paris"))

# Try select
df %>% select(Reviewer_Location)

# Get only reviews from Australia
df %>% 
  filter(Reviewer_Location == "Australia")

# Arrange by Review_ID
df %>% 
  arrange(Review_ID)
