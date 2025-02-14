# Load required libraries
library(tidyverse)
library(lubridate)

# Define data directory
DATA_DIR <- "outputs/data/raw"
PROCESSED_DIR <- "outputs/data/processed"

# Create processed directory if it doesn't exist
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

# Load data
message("Loading data...")
users <- read_rds(file.path(DATA_DIR, "users.rds"))
campaigns <- read_rds(file.path(DATA_DIR, "campaigns.rds"))
user_campaigns <- read_rds(file.path(DATA_DIR, "user_campaigns.rds"))
daily_steps <- read_rds(file.path(DATA_DIR, "daily_steps.rds"))

# Basic data validation checks
message("\nRunning data validation checks...")

# 1. Check for missing values
message("\nChecking for missing values:")
missing_counts <- list(
    users = colSums(is.na(users)),
    campaigns = colSums(is.na(campaigns)),
    user_campaigns = colSums(is.na(user_campaigns)),
    daily_steps = colSums(is.na(daily_steps))
)
print(missing_counts)

# 2. Check for duplicate records
message("\nChecking for duplicates:")
duplicates <- list(
    users = users %>% group_by(user_id) %>% filter(n() > 1) %>% nrow(),
    campaigns = campaigns %>% group_by(campaign_id) %>% filter(n() > 1) %>% nrow(),
    user_campaigns = user_campaigns %>% 
        group_by(user_health_campaign_id) %>% 
        filter(n() > 1) %>% 
        nrow(),
    daily_steps = daily_steps %>% 
        group_by(user_id, date) %>% 
        filter(n() > 1) %>% 
        nrow()
)
print(duplicates)


# 3. Check date ranges
message("\nChecking date ranges:")
date_ranges <- list(
    user_activation = range(users$activated_ts),
    campaign_enrollment = range(user_campaigns$campaign_enrolled_ts),
    campaign_completion = range(user_campaigns$campaign_completed_ts),
    daily_steps = range(daily_steps$date)
)
print(date_ranges)

# 5. Basic statistics
message("\nBasic statistics:")
stats <- list(
    total_users = nrow(users),
    total_campaigns = nrow(campaigns),
    total_participations = nrow(user_campaigns),
    total_step_records = nrow(daily_steps),
    avg_steps = mean(daily_steps$step_count, na.rm = TRUE),
    median_steps = median(daily_steps$step_count, na.rm = TRUE)
)
print(stats)

# Data cleaning based on validation results
message("\nCleaning data...")

# 1. Remove any duplicate records
daily_steps_cleaned <- daily_steps %>%
    distinct(user_id, date, .keep_all = TRUE)

# 2. Remove invalid step counts (negative or unreasonably high)
daily_steps_cleaned <- daily_steps_cleaned %>%
    filter(step_count >= 0, step_count < 100000)  # Assuming 100k steps is unreasonable

# 3. Ensure dates are within reasonable range
daily_steps_cleaned <- daily_steps_cleaned %>%
    filter(date >= min(users$activated_ts),
           date <= Sys.Date())  # No future dates


# Save cleaned data
message("\nSaving cleaned data...")
write_rds(daily_steps_cleaned, file.path(PROCESSED_DIR, "daily_steps_cleaned.rds"))

message("\nValidation and cleaning complete!")
message("Cleaned data saved to: ", PROCESSED_DIR)