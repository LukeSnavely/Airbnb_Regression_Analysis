# Loading in packages
library(tidyverse)

# Loading in the data
NY <- read_csv("data/NY_AIRBNB.csv")

# Clean version of data with selected variables
NY_clean <- NY |> 
  # We are predicting price, so no NAs
  filter(!is.na(price)) |> 
  # Selecting desired variables
  select(price, host_is_superhost, last_scraped, host_since,
         calculated_host_listings_count, neighbourhood_group_cleansed,
         room_type, accommodates, bathrooms, bedrooms,
         beds, minimum_nights, number_of_reviews, review_scores_rating) |> 
  # Removing any other NAs
  na.omit() |> 
  # Turning price into a numeric variable
  mutate(price = round(as.numeric(gsub("[$,]", "", price)), 2),
         # finding number of days the host has been a host
         days_since_host_joined = last_scraped - host_since) |> 
  # unslecting the dates
  select(!c(host_since, last_scraped))

# Structure
str(NY_clean)

# Summary of categorical 
summary(NY_clean[ , c(2, 4, 5)])

# Summary of numeric
sapply(NY_clean[, c(1, 3, 6:13)], function(x) c(summary = summary(x), sd = sd(x)))
# Visualizations

