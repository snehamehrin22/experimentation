################################################################################
# Data Validation and Cleaning Pipeline
# 
# This script performs data quality checks and cleaning on the health tracking
# dataset. It validates user activity data, campaign participation, and daily
# step counts, then produces a cleaned dataset for further analysis.
#
# Author: [Your Name]
# Last Modified: [Date]
################################################################################

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date/time operations

################################################################################
# Configuration
################################################################################

# Define data directories
DATA_DIR <- "outputs/data/raw"
PROCESSED_DIR <- "outputs/data/processed"

# Create processed directory if it doesn't exist
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# Data Loading
################################################################################

message("Loading data...")
users <- read_rds(file.path(DATA_DIR, "users.rds"))
campaigns <- read_rds(file.path(DATA_DIR, "campaigns.rds"))
user_campaigns <- read_rds(file.path(DATA_DIR, "user_campaigns.rds"))
daily_steps <- read_rds(file.path(DATA_DIR, "daily_steps.rds"))

################################################################################
# Data Validation
################################################################################

message("\nRunning data validation checks...")

#' Check for missing values across all datasets
#' Returns count of NA values per column in each dataset
message("\nChecking for missing values:")
missing_counts <- list(
    users = colSums(is.na(users)),
    campaigns = colSums(is.na(campaigns)),
    user_campaigns = colSums(is.na(user_campaigns)),
    daily_steps = colSums(is.na(daily_steps))
)
print(missing_counts)

#' Check for duplicate records based on primary keys
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

#' Validate date ranges across all temporal data
message("\nChecking date ranges:")
date_ranges <- list(
    user_activation = range(users$activated_ts),
    campaign_enrollment = range(user_campaigns$campaign_enrolled_ts),
    campaign_completion = range(user_campaigns$campaign_completed_ts),
    daily_steps = range(daily_steps$date)
)
print(date_ranges)

#' Calculate basic statistics for data overview
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

################################################################################
# Data Cleaning
################################################################################



################################################################################
# Data Completeness Analysis
################################################################################

#' Analyze data completeness per user
#' Calculates the percentage of days with step data between first and last recording
steps_continuity_analysis <- daily_steps %>% 
    group_by(user_id) %>% 
    summarize(
        total_days = n(),
        first_date = min(date),
        last_date = max(date),
        expected_days = as.numeric(difftime(last_date, first_date, units = 'days')) + 1,
        completeness = (total_days / expected_days) * 100
    ) %>% 
    ungroup()

#' Generate completeness statistics
completeness_stats <- steps_continuity_analysis %>%
    summarize(
        avg_completeness = mean(completeness),
        median_completeness = median(completeness),
        users_above_90pct = sum(completeness >= 90),
        total_users = n(),
        min_completeness = min(completeness),
        max_completeness = max(completeness)
    )
print(completeness_stats)






