# Load libraries
library(tidyverse)
library(scales) # optional

# Data source
# https://www.kaggle.com/datasets/arushchillar/disneyland-reviews?resource=download

# Load data. This file is an R data-formatted file that contains a random sample
# of 1,000 reviews and has the Park name cleanup already completed.
load("workshop_data.RData")


# ggplot2: Distribution of Ratings ----------------------------------------

# Simple histogrqam
df |> 
  ggplot(aes(Rating)) +
  geom_bar()

# Add labels for title and axes
df |> 
  ggplot(aes(Rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count")

# Add colors for the bars. Note that the `color` attribute controls the border
# color.
df |> 
  ggplot(aes(Rating)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count")

# Add a theme
df |> 
  ggplot(aes(Rating)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Count") +
  theme_minimal()

# Ratings by Park ---------------------------------------------------------

# Create a stacked bar chart. This is done by adding `fill` to the ggplot
# aesthetics.
df |> 
  ggplot(aes(Rating, fill = Park)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       fill = "Park") +
  theme_minimal()

# Move the legend position under the chart
df |> 
  ggplot(aes(Rating, fill = Park)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Ratings by Park",
       x = "Rating",
       y = "Count",
       fill = "Park") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Ratings Over Time -------------------------------------------------------

# Count the number of ratings per park per year
ratings_by_year <- df |> 
  filter(Year_Month != "missing") |> 
  mutate(Year_Month = ym(Year_Month),
         Year = year(Year_Month),
         Month = month(Year_Month)) |> 
  group_by(Park) |> 
  count(Year) |> 
  rename(Ratings = n)

ratings_by_year

# Create a line plot
ratings_by_year |> 
  ggplot(aes(Year, Ratings, color = Park)) +
  geom_line()

# Add labels
ratings_by_year |> 
  ggplot(aes(Year, Ratings, color = Park)) +
  geom_line() +
  labs(title = "Ratings by Park per Year",
       x = "Year",
       y = "Ratings")

# Add years to X-axis
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
                                2020))

# Experiment with other themes
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
  ggthemes::theme_hc()

# A density plot using ggridges
df |> 
  filter(Year_Month != "missing",
         Rating < 4) |> 
  mutate(Year_Month = ym(Year_Month),
         Year = year(Year_Month)) |> 
  group_by(Park, Year_Month) |> 
  ggplot(aes(x = Year_Month, y = Park)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01,
                                scale = 1.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Distribution of Ratings by Park",
       x = "",
       y = "") +
  theme_minimal()