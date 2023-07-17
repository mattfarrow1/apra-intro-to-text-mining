# Install libraries (only need to do once)
install.packages("tidyverse")

# Load libraries
library(tidyverse)

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# Load data
df <- read_csv("DisneylandReviews.csv")

# Look at the structure of the data
str(df)

# Using dplyr's `glimpse` function
glimpse(df)

# Look at the first n rows
head(df)

# Look at the last n rows
tail(df)

# Look at just the head of location
head(df$Reviewer_Location)

# What parks have been reviewed
unique(df$Branch)