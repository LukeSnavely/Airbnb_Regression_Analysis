# Loading in packages
library(tidyverse)
library(gt)
library(MASS)

# Loading in the data
NY <- read_csv("data/NY_AIRBNB.csv")

# Creating a themeing function, so we don't have to keep typing out our themes
regression_theme <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(size = 18,
                                face = "bold",
                                hjust = .5),
      axis.title = element_text(size = 15,
                                face = "bold",
                                hjust = .5),
      legend.title = element_text(size = 15,
                                  face = "bold")
    )
}

# Data cleaning -----------------------------------------------------------

# Clean version of data with selected variables
NY_clean <- NY |> 
  # We are predicting price, so no NAs
  filter(!is.na(price)) |> 
  # Selecting desired variables
  dplyr::select(price, host_is_superhost, last_scraped, host_since,
         calculated_host_listings_count, neighbourhood_group_cleansed,
         room_type, accommodates, bathrooms, bedrooms,
         beds, minimum_nights, number_of_reviews, review_scores_rating) |> 
  # Removing any other NAs
  na.omit() |> 
  # Turning price into a numeric variable
  mutate(price = round(as.numeric(gsub("[$,]", "", price)), 2),
         # finding number of days the host has been a host
         days_since_host_joined = as.numeric(last_scraped - host_since)) |> 
  # Getting rid of Shared Room and Hotel Room due to low observations
  filter(room_type %in% c("Entire home/apt", "Private room")) |> 
  # unslecting the dates
  dplyr::select(!c(host_since, last_scraped)) |> 
  # Filtering for prices between $25 and $1000
  filter(price >= 50,
         price <= 500)

# Price is right-skewed; let's apply the log transformation
# Prices BEFORE LOG
NY_clean |> 
  ggplot(aes(x = price)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(title = "Price: Before LOG Transformation",
       x = "Price",
       y = "Count") +
  regression_theme()

# Prices AFTER LOG
NY_clean |> 
  ggplot(aes(x = log(price))) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(title = "Price: After LOG Transformation",
       x = "LOG(Price)",
       y = "Count") +
  regression_theme()


# Transforming the data so that price is the natural log
NY_transformed <- NY_clean |> 
  mutate(lnPRICE = log(price),
         lnHOST_LISTINGS = log(calculated_host_listings_count + 1),
         lnMINIMUM_NIGHTS = log(minimum_nights + 1),
         lnREVIEWS = log(number_of_reviews + 1))

# Structure
str(NY_transformed)

# Summary of categorical 
summary(NY_transformed[ , c(2, 4, 5)])

# Summary of numeric
sapply(NY_transformed[, c(1, 3, 6:14)], function(x) c(summary = summary(x), sd = sd(x)))


# EDA: Visualizations ----------------------------------------------------------


# Accomodates
NY_clean |> 
  ggplot(aes(x = accommodates)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Accomodates",
       y = "Count") +
  regression_theme()

# Bathrooms
NY_clean |> 
  ggplot(aes(x = bathrooms)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Bathrooms",
       y = "Count") +
  regression_theme()

# Bedrooms
NY_clean |> 
  ggplot(aes(x = bedrooms)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Bedrooms",
       y = "Count") +
  regression_theme()

# Beds
NY_clean |> 
  ggplot(aes(x = beds)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Beds",
       y = "Count") +
  regression_theme()

# Calculated Listings
NY_clean |> 
  ggplot(aes(x = calculated_host_listings_count)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Calculated Host Listings",
       y = "Count") +
  regression_theme()

# Minimum Nights
NY_clean |> 
  ggplot(aes(x = minimum_nights)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Minimum Nights",
       y = "Count") +
  regression_theme()

# Number of Reviews
NY_clean |> 
  ggplot(aes(x = number_of_reviews)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Number of Reviews",
       y = "Count") +
  regression_theme()

NY_clean |> 
  ggplot(aes(x = log(number_of_reviews + 1))) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Number of Reviews",
       y = "Count") +
  regression_theme()

# Review Scores Rating
NY_clean |> 
  ggplot(aes(x = review_scores_rating)) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Review Scores Ratings",
       y = "Count") +
  regression_theme()

# Days since host joined
NY_clean |> 
  ggplot(aes(x = as.numeric(days_since_host_joined))) +
  geom_histogram(fill = "steelblue", col = "black") +
  labs(x = "Review Scores Ratings",
       y = "Count") +
  regression_theme()

# Scatterplot matrix
pairs(~calculated_host_listings_count + accommodates + bathrooms + bedrooms + beds + minimum_nights + number_of_reviews +
        review_scores_rating + days_since_host_joined + lnPRICE, 
      data = NY_transformed, upper.panel = NULL, gap = 0)

# Correlations
cors <- round(cor(NY_transformed[ , c("calculated_host_listings_count", "accommodates", "bathrooms", 
                                      "bedrooms", "beds", "minimum_nights", "number_of_reviews", "review_scores_rating",
                                      "days_since_host_joined", "lnPRICE")]),3)[10, ]

# Putting correlations in a df
correlations <- data.frame(Variable = names(cors), Correlation = as.numeric(cors))

# Making a gt table of correlations
correlations |> 
  filter(Variable != "lnPRICE") |> 
  gt() |> 
  tab_header(title = md("**Correlations with lnPRICE**")) |> 
  gtsave("Correlations.png")

# Categorical Variable Analysis
# Neighborhood group
NY_transformed |> 
  ggplot(aes(x = lnPRICE, fill = neighbourhood_group_cleansed)) +
  geom_histogram(col = "black") +
  facet_wrap(~ neighbourhood_group_cleansed, nrow = 5) +
  scale_fill_manual("Neighborhood", values = c("steelblue", "forestgreen", "gold", "grey", "darkblue")) +
  regression_theme() +
  labs(title = "Manhattan generally faces the largest Airbnb prices",
       x = "LOG of Price",
       y = "Count")

# Host is super host
NY_transformed |> 
  ggplot(aes(x = lnPRICE, fill = host_is_superhost)) +
  geom_histogram(col = "black") +
  facet_wrap(~ host_is_superhost, nrow = 2) +
  scale_fill_manual("Superhost?", values = c("steelblue", "forestgreen")) +
  regression_theme() +
  labs(title = "Status of host may not affect price",
       x = "LOG of Price",
       y = "Count")

# Room Type
NY_transformed |> 
  ggplot(aes(x = lnPRICE, fill = room_type)) +
  geom_histogram(col = "black") +
  facet_wrap(~ room_type, nrow = 4) +
  scale_fill_manual("Room Type", values = c("steelblue", "forestgreen", "gold", "grey")) +
  regression_theme() +
  labs(title = "Renting out an entire home generally \ncosts more than private rooms",
       x = "LOG of Price",
       y = "Count")



# Modeling ----------------------------------------------------------------

# Removing vars from the model to only have ln vars
model_data <- NY_transformed |> 
  dplyr::select(!c(price, number_of_reviews, calculated_host_listings_count, minimum_nights)) |> 
  filter(bathrooms < 10)
  

# Sampling 5000 rows
set.seed(412)
model_data <- model_data[sample(nrow(model_data), 5000, replace = FALSE), ]


# Full Model
model_full <- lm(lnPRICE ~ ., data = model_data)

# Null Model
model_null <- lm(lnPRICE ~ 1, data = model_data)

# Correlation matrix
pairs(~  accommodates + bathrooms + bedrooms + beds + review_scores_rating + days_since_host_joined + lnHOST_LISTINGS + lnMINIMUM_NIGHTS + lnREVIEWS + lnPRICE, data = model_data, upper.panel = NULL, gap = 0)

# Forward Selection with BIC as comparison
stepAIC(model_null, 
        # goes from null model to full model
        scope = list(lower = model_null, upper = model_full), 
        # k = log(nrow(model_data)) --> BIC (constant in the BIC formula)
        k = log(nrow(model_data)), 
        # Forward selection
        direction = "forward")

# Forward and Backward Selection
model_complete <- lm(formula = lnPRICE ~ accommodates + neighbourhood_group_cleansed + 
                               lnMINIMUM_NIGHTS + room_type + lnHOST_LISTINGS + bedrooms + 
                               review_scores_rating + host_is_superhost + bathrooms + days_since_host_joined, 
                             data = model_data)

summary(model_complete)
plot(model_complete)

# Simple model
model_simple <- lm(lnPRICE ~ accommodates + neighbourhood_group_cleansed + bedrooms + lnMINIMUM_NIGHTS, data = model_data)

plot(model_simple)
summary(model_simple)