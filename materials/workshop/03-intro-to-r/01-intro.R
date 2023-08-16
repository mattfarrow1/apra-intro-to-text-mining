
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
  filter(bill_length_mm > 40)
penguins |> 
  filter(bill_length_mm >= 40)

# By missing values
penguins |> 
  filter(is.na(flipper_length_mm))
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
penguins |>
  mutate(flipper_length = case_when(
    flipper_length_mm < 190 ~ "short",
    flipper_length_mm >= 190 &
      flipper_length_mm < 213 ~ "average",
    flipper_length_mm > 213 ~ "long"
  )) |>
  select(flipper_length_mm, flipper_length) |>
  head()

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

# Intro to ggplot2 --------------------------------------------------------

# We looked at a plot briefly earlier, but let's do a little more plotting so
# that you have a general understanding of how it works
penguins |> 
  ggplot(aes(flipper_length_mm)) +
  geom_bar() 

penguins |> 
  ggplot(aes(bill_length_mm,
             flipper_length_mm,
             color = species)) +
  geom_point(size = 3, alpha = 0.4) +
  labs(title = "Bill vs. Flipper Length",
       subtitle = "by species",
       x = "Bill Length (mm)",
       y = "Flipper Length (mm)",
       color = "") +
  theme_minimal()

