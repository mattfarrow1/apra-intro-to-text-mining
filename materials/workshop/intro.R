
# Setup -------------------------------------------------------------------

# Install libraries (only need to do once)
# install.packages("tidyverse")
# install.packages("palmerpenguins")

# Load libraries
library(tidyverse)
library(palmerpenguins)

# Examine the Data --------------------------------------------------------

# Look at the data. If you don't want to load the entire library, you can call
# just one function by using package_name::function_name
head(palmerpenguins::penguins)

# You'll notice there are 344 rows and 8 columns. Everything doesn't fit nicely
# in the console, but we'll look at some other ways to view the data in a
# minute. For now, here is how you can view the names of the columns/variables.
colnames(penguins)

# dplyr offers the `glimpse` function which offers a more readable format than
# `head` in the console
glimpse(penguins)

# Two additional ways that you can view the data are `str` and `view`.
str(penguins)
view(penguins)

# What if we want to find out which species are included in the data?
unique(penguins$species)

# Intro to dplyr ----------------------------------------------------------

# How many are male?
filter(penguins, sex == "male")

# How many are male on Biscoe island? 
filter(penguins, sex == "male", island == "biscoe")

# Why are there no results? R is case-sensitive and a single letter being wrong
# can break your code. In this case we didn't capitalize "Biscoe"
# How many are male on Biscoe island? 
filter(penguins, sex == "male", island == "Biscoe")

# What if we wanted to sort our female penguins by flipper length? Now we have
# two actions. This is where the pipe operator comes in! Written as either %>%
# or |>, this allow us to chain operations together and is where the design of
# the tidyverse is incredibly helpful.
penguins |> 
  filter(sex == "female") |> 
  arrange(flipper_length_mm)

# Some other ways to filter:

# Using boolean operators
penguins |> 
  filter(bill_length_mm < 40)
penguins |> 
  filter(bill_length_mm >= 40)

# By missing values
# Flipper length is missing/blank
penguins |> 
  filter(is.na(flipper_length_mm))
# Flipper length is NOT missing/blank
penguins |> 
  filter(!is.na(flipper_length_mm))

# Now let's make a new variable based on flipper length that groups them into
# buckets of "short", "average", and "long". First, let's make a box plot of the
# flipper length.
penguins |> 
  ggplot(aes(flipper_length_mm)) +
  geom_boxplot() +
  theme_minimal()

# We'll use the mutate function to create a new variable, and the case_when
# function from dplyr to create our buckets. Finally, we'll select only the
# original flipper length in mm and the new bucket columns and look at the first
# observations using `head`
flippers <- penguins |>
  mutate(flipper_length = case_when(
    flipper_length_mm < 190 ~ "short",
    flipper_length_mm >= 190 &
      flipper_length_mm < 213 ~ "average",
    flipper_length_mm > 213 ~ "long"
  )) 

flippers |>
  select(flipper_length_mm, flipper_length) |>
  head()

# If we want to count the number of penguins by flipper length, we can use the
# `count` function
flippers |> 
  count(flipper_length)

# Finally let's look at the average flipper length by species. First we'll group
# by the species and then summarise by the mean (average) flipper length
penguins |> 
  group_by(species) |> 
  summarise(avg_flipper_length = mean(flipper_length_mm))

# We get a result for Chinstrap penguins, but not Adelie or Gentoo. What
# happened? if you remember when we looked at the data, there are some rows with
# missing observations (denoted by NA). We need to tell R to summarise the data
# and exclude those missing values
penguins |> 
  group_by(species) |> 
  summarise(avg_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

# Our Data ----------------------------------------------------------------

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
df <- df |> 
  mutate(Branch = case_match(Branch, 
                             "Disneyland_California" ~ "California",
                             "Disneyland_HongKong" ~ "Hong Kong",
                             "Disneyland_Paris" ~ "Paris")) |> 
  rename("Park" = Branch)

# Look at the final data set
glimpse(df)
