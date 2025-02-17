# ============================================================================
# Synthetic Health Data Generation Script
# Purpose: Generate realistic user, campaign, and step count data for analysis
# Author: Sneha Mehrin
# ============================================================================

# Required Libraries
library(tidyverse)    # For data manipulation and visualization
library(lubridate)    # For date/time operations
library(uuid)         # For generating unique identifiers
library(purrr)        # For functional programming operations

# Global Configuration
set.seed(42)         # Set seed for reproducibility
n_users <- 5000      # Number of users to generate
n_campaigns <- 3     # Number of health campaigns
n_fact_records <- 10000  # Number of campaign enrollment records

# ============================================================================
# 1. Dimension Tables Generation
# ============================================================================

# Generate User Dimension Table
# Contains demographic and geographic information for each user
dim_user <- tibble(
    user_id = replicate(n_users, UUIDgenerate()),
    activated_ts = sample(seq.Date(from = ymd('2020-01-01'),
                                 to = ymd('2024-01-01'),
                                 by = 'day'), size = n_users, replace = TRUE) %>% 
                  as_datetime() + hours(sample(8:20, n_users, TRUE)),
    sex = sample(c('female','male','unknown'), 
                size = n_users, 
                prob = c(0.70, 0.28, 0.02),    # Realistic gender distribution
                replace = TRUE),
    age_group = sample(c('18-24','25-34','35-44','45-54','55+'), 
                      size = n_users,
                      prob = c(0.10, 0.15, 0.35, 0.30, 0.10),  # Age distribution
                      replace = TRUE),
    population_density = sample(
        x = c(sample(3000:5000, n_users * 0.20, replace = TRUE),  # Urban
              sample(1000:2999, n_users * 0.15, replace = TRUE),  # Suburban
              sample(5:999, n_users * 0.65, replace = TRUE)),     # Rural
        size = n_users,
        replace = TRUE
    )
)

# Generate Health Campaign Dimension Table
# Defines different types of step challenges and their durations
dim_health_campaign <- tibble(
    campaign_id = replicate(n_campaigns, UUIDgenerate() %>% str_replace_all("-", "")),
    campaign_name = paste("Step Challenge", c("Basic", "Pro", "Advanced")),
    num_campaign_activities = c(15, 30, 60)  # Duration in days for each campaign
)

# ============================================================================
# 2. Fact Tables Generation
# ============================================================================

# Select users for experimental group (70% of total users)
experimental_users <- sample(dim_user$user_id, 
                           size = floor(n_users * 0.7), 
                           replace = FALSE)

# Generate Campaign Enrollment Facts
# Records which users participated in which campaigns and when
fact_user_health_campaign <- tibble(
    user_health_campaign_id = replicate(n_fact_records, UUIDgenerate() %>% str_replace_all("-", "")),
    user_id = sample(experimental_users, n_fact_records, replace = TRUE),
    campaign_id = sample(dim_health_campaign$campaign_id, n_fact_records, replace = TRUE)
) %>%
    left_join(dim_user %>% select(user_id, activated_ts), by = "user_id") %>%
    left_join(dim_health_campaign %>% select(campaign_id, num_campaign_activities), by = "campaign_id") %>%
    arrange(user_id, campaign_id) %>%
    group_by(user_id, campaign_id) %>%
    mutate(
        # Initial random enrollment dates with 60-day buffer after activation
        campaign_enrolled_ts = {
            start_date = first(activated_ts) + days(60)  # Add 60-day buffer
            end_date = as_datetime("2024-01-01")
            
            if (start_date <= end_date) {
                date_seq = seq(start_date, end_date, by = "day")
                if (length(date_seq) >= n()) {
                    sample(date_seq, n(), replace = FALSE)
                } else {
                    sample(date_seq, n(), replace = TRUE)
                }
            } else {
                rep(start_date, n())
            }
        },
        campaign_completed_ts = campaign_enrolled_ts + days(num_campaign_activities)
    ) %>%
    # Now adjust enrollment dates to ensure they come after previous completion
    mutate(
        campaign_enrolled_ts = case_when(
            row_number() == 1 ~ campaign_enrolled_ts,
            # For subsequent enrollments, ensure they're after previous completion
            TRUE ~ pmax(campaign_enrolled_ts, lag(campaign_completed_ts) + days(1))
        ),
        campaign_completed_ts = campaign_enrolled_ts + days(num_campaign_activities)
    ) %>%
    ungroup() %>%
    select(-activated_ts)

# Generate Daily Step Count Facts
# Creates realistic step count data with various factors affecting the counts:
# - Weekend vs. weekday patterns
# - Seasonal variations
# - Campaign participation effects
# - Individual user activity levels
fact_daily_steps <- user_campaigns %>%
    # Get the first campaign enrollment and last campaign completion date for each user
    group_by(user_id) %>%
    summarise(
        first_campaign_start = min(campaign_enrolled_ts),
        max_campaign_completed_ts = max(campaign_completed_ts)
    ) %>%
    # Join with user activation dates
    left_join(users %>% select(user_id, activated_ts), by = "user_id") %>%
    # Create date range including pre and post treatment periods
    mutate(
        start_date = as_date(first_campaign_start) - days(60),
        end_date = as_date(max_campaign_completed_ts) + days(60)
    ) %>%
    # Create all possible user-date combinations using crossing
    crossing(date = seq(min(start_date), max(end_date), by = "day")) %>%
    # Only keep dates within user's range
    filter(date >= start_date, date <= end_date) %>%
    # Add period flags
    mutate(
        period = case_when(
            date >= (as_date(first_campaign_start) - days(60)) & 
            date < as_date(first_campaign_start) ~ "pre_treatment",
            
            date > as_date(max_campaign_completed_ts) & 
            date <= (as_date(max_campaign_completed_ts) + days(60)) ~ "post_treatment",
            
            TRUE ~ "treatment"
        )
    ) %>%
    # Generate step counts
    mutate(
        # Base step count with campaign completion effect
        step_count = case_when(
            # Weekend steps tend to be lower
            weekdays(date) %in% c("Saturday", "Sunday") ~ 
                sample(4000:8000, n(), replace = TRUE),
            # Weekday steps
            TRUE ~ sample(6000:12000, n(), replace = TRUE)
        ),
        
        # Add some random variation
        step_count = step_count + sample(-1000:1000, n(), replace = TRUE),
        
        # Seasonal variation
        step_count = case_when(
            month(date) %in% c(12, 1, 2) ~ # Winter
                round(step_count * sample(seq(0.7, 0.9, 0.1), n(), replace = TRUE)),
            month(date) %in% c(6, 7, 8) ~ # Summer
                round(step_count * sample(seq(1.1, 1.3, 0.1), n(), replace = TRUE)),
            TRUE ~ step_count
        ),
        
        # Some users are naturally more/less active
        step_count = step_count * (1 + sample(seq(-0.2, 0.2, 0.1), n_distinct(user_id), 
                                            replace = TRUE)[as.factor(user_id)]),
        
        # Increase steps for dates after campaign completion
        step_count = case_when(
            date > as_date(max_campaign_completed_ts) ~ 
                round(step_count * sample(seq(1.2, 1.4, 0.1), n(), replace = TRUE)),
            TRUE ~ step_count
        )
    ) %>%
    # Round steps to integers and ensure no negative values
    mutate(
        step_count = pmax(0, round(step_count))
    ) %>%
    # Remove unnecessary columns
    select(user_id, date,period, step_count) %>%
    # Before the final summarize, randomly drop entire dates
    group_by(user_id) %>%
    slice_sample(prop = 0.80) %>%  # Keep 90% of dates randomly (drop 10%)
    # Continue with existing summarization
    group_by(user_id, date, period) %>%
    summarize(step_count = mean(step_count)) %>%
    ungroup() %>%
    arrange(user_id, date)


# Save each dataset to .rds files


write_rds(dim_user, "outputs/data/raw/users.rds")
write_rds(dim_health_campaign, "outputs/data/raw/campaigns.rds")
write_rds(fact_user_health_campaign, "outputs/data/raw/user_campaigns.rds")
write_rds(fact_daily_steps, "outputs/data/raw/daily_steps.rds")




