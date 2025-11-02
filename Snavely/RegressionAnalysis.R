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
         days_since_host_joined = as.numeric(last_scraped - host_since)) |> 
  # unslecting the dates
  select(!c(host_since, last_scraped))

# Structure
str(NY_clean)

# Summary of categorical 
summary(NY_clean[ , c(2, 4, 5)])

# Summary of numeric
sapply(NY_clean[, c(1, 3, 6:13)], function(x) c(summary = summary(x), sd = sd(x)))


# Visualizations ----------------------------------------------------------


# Creating a theming function, so we don't have to keep typing out our themes
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

# Transforming the data
NY_transformed <- NY_clean |> 
  mutate(lnPRICE = log(price))

# Scatterplot matrix
pairs(~calculated_host_listings_count + accommodates + bathrooms + beds + minimum_nights + number_of_reviews +
        review_scores_rating + days_since_host_joined + lnPRICE, 
      data = NY_transformed, upper.panel = NULL, gap = 0)

# Correlation
cors <- round(cor(NY_transformed[ , c("calculated_host_listings_count", "accommodates", "bathrooms", "beds", "minimum_nights", 
                        "number_of_reviews", "review_scores_rating", "days_since_host_joined", "lnPRICE")]),3) [9, ]

# Put in a data frame
correlations <- data.frame(Variable = names(cors), Correlation = as.numeric(cors))

correlations |> 
  filter(Variable != "lnPRICE") |> 
  gt() |> 
  tab_header(title = md("**Correlations with lnPRICE**"))

# Categorical Variable Analysis
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

# Example of categorical regression
lm <- lm(lnPRICE ~ neighbourhood_group_cleansed, data = NY_transformed)
summary(lm)
