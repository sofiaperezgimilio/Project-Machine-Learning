
#-------------AIRBNB MADRID--------------

#Import libraries
install.packages("tidyverse")
library("tidyverse")

#Import dataset
df_raw <- read.csv("data/AirBnbMadrid.csv")

#Cleaning

#1) Removing columns with NAs
percentage_na <- colMeans(is.na(df_raw))
empty_cols <- names(percentage_na[percentage_na > 0.90]) 
#columns in which the percentage of empty values is > 90%, we eliminate them

df_clean <- df_raw %>% select(-all_of(empty_cols))

ncol(df_raw)
ncol(df_clean)

#2) Columns of ids, urls and text that are not useful for predicting
cols_text_id <- c("listing_url", "scrape_id", "last_scraped", "source",
                   "name", "description", "picture_url", "host_url",
                   "host_profile_id", "host_profile_url", "host_name",
                   "host_about", "host_picture_url", "host_location",
                   "calendar_last_scraped", "license")

df_clean <- df_clean %>% select(-all_of(cols_text_id))

#3) Columns with price, we remove them
cols_with_price <- c("price_quote_checkin_date", "price_quote_checkout_date",
                  "price_quote_total_price", "price_quote_price_per_night",
                  "price_quote_raw", "estimated_revenue_l365d")

#If we want to predict price, we cannot use these cols that already contain price (it would be cheating)

df_clean <- df_clean %>% select(-all_of(cols_with_price))

ncol(df_clean)

#3) Convert price into numeric
df_clean <- df_clean %>%
  mutate(price = parse_number(as.character(price)))

summary(df_clean$price)

#Eliminate prices with NAs
df_clean <- df_clean %>% filter(!is.na(price))

summary(df_clean$price)

#Outliers
df_clean <- df_clean %>% filter(price >= 10, price <= 1000)
summary(df_clean$price)


#4) Booleans "t"/"f" -> 0/1
cols_boolean <- c("host_is_superhost", "host_has_profile_pic",
             "host_identity_verified", "has_availability")

df_clean <- df_clean %>%
  mutate(across(any_of(cols_boolean), ~ as.integer(. == "t")))

#5) Bathrooms ("1 bath", "1.5 baths"...)
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

#6) Bedrooms
table(df_clean$bedrooms) 
table(df_clean$beds) 

#Nearly all airbnbs have between 6-10 bedrooms; 8 more than 10, so we have limitated them to 10 because they can be outliers and give errors
#The same with beds

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

#7) Reviews (convert NA to 'without review')
df_clean <- df_clean %>%
  mutate(
    has_reviews = as.integer(number_of_reviews > 0), #more than 1 review -> 0; no reviews -> 0
    first_review = as.Date(first_review), #convert to Date
    last_review  = as.Date(last_review),
    years_since_first_review = as.numeric(as.Date("2026-09-21") - first_review) / 365 #calculates how many years have passed since the first review
  ) %>%
  select(-first_review, -last_review)









