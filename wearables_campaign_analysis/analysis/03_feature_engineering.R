# ============================================================================
# Feature Engineering for Daily Steps Analysis
# 
# This script processes user step data to create a complete daily steps dataset
# with proper handling of missing values and period classification.
# ============================================================================

# -----------------------------
# 1. Create Complete Daily Steps Dataset
# -----------------------------
fact_daily_steps_complete <- user_campaigns %>%
    # Get user-specific campaign enrollment periods
    group_by(user_id) %>%
    summarise(
        first_enrolled_date = min(campaign_enrolled_ts),
        last_completed_date = max(campaign_completed_ts)
    ) %>%
    # Create 60-day windows before and after campaign periods
    mutate(
        start_date = as_date(first_enrolled_date) - days(60),
        end_date = as_date(last_completed_date) + days(60)
    ) %>%
    # Generate complete date sequence for each user
    group_by(user_id) %>%
    tidyr::expand(
        nesting(first_enrolled_date, last_completed_date),
        date = seq(start_date, end_date, by = "day")
    ) %>%
    # Classify periods as pre-treatment, treatment, or post-treatment
    mutate(
        period = case_when(
            date < first_enrolled_date ~ "pre_treatment",
            date > last_completed_date ~ "post_treatment",
            TRUE ~ "treatment"
        )
    ) %>%
    ungroup() %>% 
    # Merge with actual step data and handle missing values
    left_join(
        fact_daily_steps %>% 
        select(-period) %>% 
        mutate(step_count = as.numeric(step_count)), 
        by = c("user_id", "date")
    ) %>%
    mutate(step_count = replace(step_count, is.na(step_count), 0))

# -----------------------------
# 2. Identify Eligible Users
# -----------------------------
# Define eligible users as those with no gaps > 7 days in their data
eligible_users <- fact_daily_steps_complete %>%
    filter(!is.na(step_count)) %>%
    group_by(user_id) %>%
    arrange(user_id, date) %>%
    mutate(
        date_diff = as.numeric(date - lag(date))
    ) %>%
    summarise(
        max_gap = max(date_diff, na.rm = TRUE)
    ) %>%
    filter(max_gap <= 7) %>% 
    pull(user_id)

# -----------------------------
# 3. Process Final Dataset
# -----------------------------
user_daily_steps_final <- fact_daily_steps_complete %>%
    # Filter to eligible users only
    filter(user_id %in% eligible_users) %>%
    # Calculate 7-day rolling mean and impute missing values
    group_by(user_id) %>%
    arrange(date) %>%
    mutate(
        rolling_mean = round(zoo::rollmean(
            step_count, 
            k = 7, 
            fill = 'extend',
            align = "center"
        )),
        # Impute zeros with rolling mean
        step_count = if_else(
            step_count == 0 & !is.na(rolling_mean),
            rolling_mean,
            step_count
        )
    ) %>%
    # Select relevant columns and join with user data
    select(user_id, date, period, step_count) %>%
    ungroup() %>% 
    inner_join(users, by = 'user_id') %>% 
    select(-activated_ts) %>% 
    # Remove treatment period data
    filter(period != 'treatment')

# Save processed dataset
write_rds(user_daily_steps_final, 'outputs/data/processed/user_daily_steps_final.rds')

.