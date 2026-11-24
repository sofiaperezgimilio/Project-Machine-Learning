
#-------------AIRBNB MADRID--------------

#Import libraries
#install.packages("tidyverse")
#install.packages("jsonlite")
library("tidyverse")
library("jsonlite")

#Import dataset
df_raw <- read.csv("data/AirBnbMadrid.csv")

#Cleaning

#1. Removing columns with NAs
percentage_na <- colMeans(is.na(df_raw))
empty_cols <- names(percentage_na[percentage_na > 0.90]) 
#columns in which the percentage of empty values is > 90%, we eliminate them

df_clean <- df_raw %>% select(-all_of(empty_cols))

ncol(df_raw)
ncol(df_clean)

# 2. Remove not necessary columns.

#Columns of ids, urls and text that are not useful for predicting
cols_text_id <- c("id", "host_id", "listing_url", "scrape_id", "last_scraped", "source",
                  "name", "description", "picture_url", "host_url",
                  "host_profile_id", "host_profile_url", "host_name",
                  "host_about", "host_picture_url", "host_location",
                  "calendar_last_scraped", "license")

#Property type. There are lots of categories, so it's useless. We can have similar information with room_type.

#Reviews. There are lots of variables related to this, some of which are not necessary, we will only keep the most important ones. Number of reviews,
#review_scores_rating (this is the one that groups other ones) and reviews_per_month.

#Host listing count. We will keep only the general one, and remove the ones that are specific for each listing type.

#Minimum maximum nights. We will only keep the ones that are useful. The minimum and maximum nigths needed to make a booking.

#Amenities. The values for this columns are lists with all the amenities, which are too long. We will only keep the number of amenities.

#Availability. There are some columns showing the available days for the next 30, 60, 90, 365 days and untill the end of the year. We only will keep one.
#The estimated occupancy is an estimation, not extracted data.

#Neighbourhood. We will only keep grouped one, as there are less categories, that can be more useful. 

#Hosts time as host/user. There are 2 redundant variables related to this ones, as it's in years and months. We will use the one related to host.

#Profile pic. We will remove this variable, as it doesn't give us interesting information. Having a profile pic doesn't give necessarily confidence
#as it can be a landscape, a fake picture... There are other variables that give more information that can give us confidence.

#Host verifications has all the values as "None".

table(df_clean$host_verifications)

func <- function(x) {
  if (is.na(x) || x == "" || x == "[]") {
    return(0)
  } else {
    return(length(fromJSON(x)))
  }
}

df_clean <- df_clean %>% mutate(n_amenities = sapply(df_clean$amenities, func, USE.NAMES = F))
useless_cols <- c("property_type", "number_of_reviews_ltm", "number_of_reviews_l30d", "number_of_reviews_ly", "review_scores_accuracy",
                  "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", "review_scores_location", "review_scores_value",
                  "calculated_host_listings_count", "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms",
                  "calculated_host_listings_count_shared_rooms", "minimum_minimum_nights", "maximum_minimum_nights", "minimum_maximum_nights",
                  "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm", "amenities", "availability_30", "availability_60",
                  "availability_90", "availability_365", "availability_eoy", "neighbourhood_cleansed", "hosts_time_as_user_months", "hosts_time_as_host_months",
                  "hosts_time_as_user_years", "estimated_occupancy_l365d", "host_has_profile_pic", "host_verifications")

#Columns with price, we will only keep the price per night. If we want to predict this, we cannot use these cols that already contain price (it would be cheating)

cols_with_price <- c("price_quote_checkin_date", "price_quote_checkout_date",
                  "price_quote_total_price", "price_quote_price_per_night",
                  "price_quote_raw", "estimated_revenue_l365d")

cols_to_remove <- c(cols_text_id, useless_cols, cols_with_price)


df_clean <- df_clean %>% select(-any_of(cols_to_remove))

ncol(df_clean)

# Convert price into numeric
df_clean <- df_clean %>%
  mutate(price = parse_number(as.character(price)))

summary(df_clean$price)

#Eliminate prices with NAs
df_clean <- df_clean %>% filter(!is.na(price))

summary(df_clean$price)

#Outliers
#We have set an upper limit to avoid distortions. We only keep prices below 1000 per night, so we remove 164 observations.
sum(df_clean$price > 1000)
df_clean <- df_clean %>% filter(price <= 1000)
summary(df_clean$price)


#3. Booleans "t"/"f" -> 0/1
cols_boolean <- c("host_is_superhost","host_identity_verified", "has_availability")

df_clean <- df_clean %>%
  mutate(across(any_of(cols_boolean), ~ as.integer(. == "t")))

#4. Bathrooms ("1 bath", "1.5 baths"...)
df_clean <- df_clean %>%
  mutate(
    bath_text = str_to_lower(bathrooms_text), #Shared = shared
    bath_shared = as.integer(str_detect(bath_text, "shared")), #if contains 'shared' -> 1 if not -> 0
    bath_num = parse_number(bath_text), #get the number
    bath_num = if_else(is.na(bath_num) & str_detect(bath_text, "half"),
                           0.5, bath_num) #'half-bath' does not have a number -> 0.5
  ) %>%
  select(-bathrooms_text, -bath_text) %>%
  rename(bathrooms = bath_num)

summary(df_clean$bathrooms)

#5. Bedrooms
table(df_clean$bedrooms) 
table(df_clean$beds) 

# 9.96% of the airbnbs have 0-10 bedrooms, and only 8 listings exceed 10 (up to 50).
bedrooms0_10 <- mean(df_clean$bedrooms <= 10, na.rm = TRUE)
bedrooms0_10

#99.89% of the airbnbs have 1-16 beds, and 20 listings exceed 16
beds1_16 <- mean(df_clean$beds <= 16, na.rm = TRUE)
beds1_16

#We limit bedrooms at 10 and beds at 16 to avoid extreme outliers distorting.
#NA values we have applied imputation of the median
df_clean <- df_clean %>%
  mutate(
    bedrooms = pmin(bedrooms, 10),
    beds     = pmin(beds, 16),
    bedrooms = if_else(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms),
    beds     = if_else(is.na(beds),     median(beds,     na.rm = TRUE), beds),
    bathrooms = if_else(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms)
  )

summary(df_clean$bedrooms)
summary(df_clean$beds)
summary(df_clean$bathrooms)

#6. Reviews (convert NA to 'without review')
df_clean <- df_clean %>%
  mutate(
    has_reviews = as.integer(number_of_reviews > 0), #more than 1 review -> 0; no reviews -> 0
    first_review = as.Date(first_review), #convert to Date
    last_review  = as.Date(last_review),
    years_since_first_review = as.numeric(as.Date("2026-09-21") - first_review) / 365 #calculates how many years have passed since the first review
  ) %>%
  select(-first_review, -last_review)

#TRAIN/TEST SPLITTING









