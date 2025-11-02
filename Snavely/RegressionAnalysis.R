# Loading in packages
library(tidyverse)

# Loading in the data
NY <- read_csv("data/NY_AIRBNB.csv")

# Clean version of data with selected variables
NY_clean <- NY |> 
  # We are predicting price, so no NAs
  filter(!is.na(price)) |> 
  # Selecting desired variables
  select(price, host_is_superhost,
         calculated_host_listings_count, neighbourhood_group_cleansed,
         room_type, accommodates, bathrooms, bedrooms,
         beds, minimum_nights, number_of_reviews, review_scores_rating) |> 
  # Removing any other NAs
  na.omit()

str(NY_clean)
