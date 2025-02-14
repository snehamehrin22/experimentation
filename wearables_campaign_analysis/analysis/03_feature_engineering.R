# Script: Step Count Feature Engineering for Health Campaign Analysis
# Purpose: Prepare daily step count data by handling missing values and creating treatment periods
# Author: [Your Name]
# Last Modified: [Date]

# 1. Missing Data Quality Check
# ---------------------------
gap_analysis_results <- daily_steps_cleaned %>% 
  group_by(user_id) %>% 
  mutate(consecutive_day_gap = as.numeric(date - lag(date))) %>%  
  ungroup() %>% 
  select(user_id, consecutive_day_gap) %>% 
  filter(consecutive_day_gap > 1 & consecutive_day_gap < 7) %>% 
  distinct(user_id) %>% 
  nrow()

# 2. Cohort Definition
# ------------------
# Define cohort of users with consistent data recording (gaps <= 7 days)
eligible_users <- daily_steps_cleaned %>%
  arrange(user_id, date) %>%
  group_by(user_id) %>%
  mutate(
    consecutive_day_gap = as.numeric(date - lag(date)),
    max_gap_days = if(all(is.na(consecutive_day_gap))) 0 else max(consecutive_day_gap, na.rm = TRUE)
  ) %>%
  filter(max_gap_days <= 7, sum(!is.na(step_count)) >= 2) %>%
  pull(user_id) %>%
  unique()

# 3. Time Series Completion and Imputation
# -------------------------------------
# Create complete daily series and impute missing values using 7-day moving average
step_count_completed <- daily_steps_cleaned %>%
  filter(user_id %in% eligible_users) %>%
  arrange(user_id, date) %>%
  group_by(user_id) %>%
  summarize(
    observation_start_date = min(date),
    observation_end_date = max(date)
  ) %>% 
  ungroup() %>% 
  group_by(user_id) %>% 
  # Generate complete daily sequence
  expand(date = seq.Date(observation_start_date, observation_end_date, by = 'day')) %>% 
  ungroup() %>% 
  # Merge with original data and apply imputation
  left_join(daily_steps_cleaned, by = c('user_id', 'date')) %>% 
  mutate(step_count = case_when(
    is.na(step_count) ~ zoo::rollmean(step_count, k = 7, fill = "extend", align = "center"),
    TRUE ~ step_count
  )) %>%  
  # Join demographic data and create temporal features
  left_join(dim_user, by = 'user_id') %>% 
  mutate(
    month_factor = month(date) %>% as_factor(),
    day_of_week = weekdays(date) %>% as_factor()
  )

# 4. Campaign Period Classification
# ------------------------------
# Classify observations into pre/during/post campaign periods
step_count_with_campaign_periods <- step_count_completed %>% 
  left_join(
    fact_user_health_campaign %>% 
      group_by(user_id) %>% 
      summarize(
        campaign_start_date = as.Date(min(campaign_enrolled_ts)),
        campaign_end_date = as.Date(max(campaign_completed_ts))
      ) %>% 
      ungroup(), 
    by = 'user_id'
  ) %>% 
  mutate(campaign_period = case_when(
    date < campaign_start_date ~ 'pre_campaign',
    date > campaign_end_date ~ 'post_campaign',
    TRUE ~ 'during_campaign'
  ))

# Output validation
cat(sprintf("Total eligible users: %d\n", length(eligible_users)))
cat(sprintf("Date range: %s to %s\n", 
    min(step_count_with_campaign_periods$date),
    max(step_count_with_campaign_periods$date)))

# Create treatment groups and calculate enrollment dates
# This step joins campaign enrollment data and classifies each day into treatment periods
daily_treatment_group <- step_count_with_campaign_periods %>% 
  # Join with campaign enrollment dates for each user
  left_join(
    fact_user_health_campaign %>% 
      group_by(user_id) %>% 
      # Get the first enrollment and last completion dates for each user
      summarize(
        first_enrolled_date = as.Date(min(campaign_enrolled_ts)),
        last_completed_date = as.Date(max(campaign_completed_ts))
      ) %>% 
      ungroup(), 
    by = 'user_id'
  ) %>% 
  # Classify each day into one of three treatment periods:
  # - pre-treatment: before campaign enrollment
  # - treatment: during campaign
  # - post-treatment: after campaign completion
  mutate(treatment_group = case_when(
    date < first_enrolled_date ~ 'pre-treatment-period',
    date > last_completed_date ~ 'post-treatment-period',
    TRUE ~ 'treatment-period'
  ))

# Filter and prepare data for pre/post treatment analysis
# This creates a dataset focused on the 60-day windows before and after treatment
daily_treatment_group_filtered <- daily_treatment_group %>%
  group_by(user_id, treatment_group) %>%
  arrange(user_id, date) %>%
  # Calculate days relative to treatment start/end:
  # - For pre-treatment: days until enrollment
  # - For post-treatment: days since completion
  # - During treatment: set to 0
  mutate(
    days_from_start = case_when(
      treatment_group == "pre-treatment-period" ~ as.numeric(first_enrolled_date - date),
      treatment_group == "post-treatment-period" ~ as.numeric(date - last_completed_date),
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>% 
  # Keep only pre/post periods within 60 days of treatment
  filter(days_from_start <= 60, treatment_group != 'treatment-period')

# Select the columns to be used for the final dataset
  user_daily_steps_final  <- daily_treatment_group_filtered  %>% select(user_id, date, treatment_group, step_count, age_group, sex, population_density, month_factor, day_of_week)  %>% 
  mutate(age_group = factor(age_group, levels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65+'), ordered = TRUE))

  # Save the processed data frame to RDS file
saveRDS(daily_treatment_group_filtered, file = "outputs/data/processed/daily_treatment_analysis.rds")
saveRDS(daily_treatment_group_filtered, file = "outputs/data/processed/user_daily_steps_final.rds")





