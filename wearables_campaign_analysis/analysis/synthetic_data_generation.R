library(tidyverse)
library(lubridate)
library(uuid)
library(purrr)

# Set seed for reproducibility
set.seed(42)

# 1. Create User Table

n_users  <-  5000
n_campaigns  <- 3

dim_user <- tibble(
    user_id = replicate(n_users, UUIDgenerate()),
    activated_ts = sample(seq.Date(from = ymd('2020-01-01'),
                                 to = ymd('2024-01-01'),
                                 by = 'day'), size = n_users, replace = TRUE) %>% 
                  as_datetime() + hours(sample(8:20, n_users, TRUE)),
    sex = sample(c('female','male','unknown'), 
                size = n_users, 
                prob = c(0.70, 0.28, 0.02), 
                replace = TRUE),
    age_group = sample(c('18-24','25-34','35-44','45-54','55+'), 
                      size = n_users,
                      prob = c(0.10, 0.15, 0.35, 0.30, 0.10),  
                      replace = TRUE),
    population_density = sample(
        x = c(sample(3000:5000, n_users * 0.20, replace = TRUE), 
              sample(1000:2999, n_users * 0.15, replace = TRUE),
              sample(5:999, n_users * 0.65, replace = TRUE)),
        size = n_users,
        replace = TRUE
    )
)

dim_health_campaign <- tibble(
    campaign_id = replicate(n_campaigns, UUIDgenerate() %>% str_replace_all("-", "")),
    campaign_name = paste("Step Challenge", c("Basic", "Pro", "Advanced")),
    num_campaign_activities = c(15, 30, 60)
)

experimental_users  <- sample(dim_user$user_id, size = floor(n_users * 0.7), replace = FALSE)
n_fact_records  <-  10000

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




# Create daily step data with positive campaign effect

fact_daily_steps <- dim_user %>%
    # Start with user and their activation dates
    select(user_id, activated_ts) %>%
    # Join with campaign data to get last completion date
    left_join(
        fact_user_health_campaign %>%
            group_by(user_id) %>%
            summarise(last_campaign_end = max(campaign_completed_ts)),
        by = "user_id"
    ) %>%
    # Create date range until 60 days after last campaign (or use original end date if user had no campaigns)
    mutate(
        end_date = if_else(
            !is.na(last_campaign_end),
            as_date(last_campaign_end) + days(60),
            ymd("2024-01-01")
        )
    ) %>%
    # Create all possible user-date combinations up to the calculated end date
    crossing(date = seq(min(as_date(activated_ts)), max(.$end_date), by = "day")) %>%
    # Only keep dates after user activation and before end_date
    filter(date >= as_date(activated_ts), date <= end_date) %>%
    # Randomly drop some days (device not connected)
    group_by(user_id) %>%
    slice_sample(prop = 0.8) %>%  # Keep 80% of days randomly
    ungroup() %>%
    # Generate step counts
    mutate(
        # Base step count
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
                                            replace = TRUE)[as.factor(user_id)])
    ) %>%
    # Add campaign effect
    left_join(
        fact_user_health_campaign %>%
            select(user_id, campaign_enrolled_ts, campaign_completed_ts),
        by = "user_id"
    ) %>%
    group_by(user_id) %>%
    mutate(
        # Increase steps during and after campaign
        campaign_multiplier = case_when(
            date >= as_date(campaign_enrolled_ts) & date <= as_date(campaign_completed_ts) ~ 
                sample(seq(1.2, 1.4, 0.1), 1),  # 20-40% increase during campaign
            date > as_date(campaign_completed_ts) ~ 
                sample(seq(1.1, 1.3, 0.1), 1),  # 10-30% increase after campaign
            TRUE ~ 1.0
        ),
        step_count = round(step_count * campaign_multiplier)
    ) %>%
    ungroup() %>%
    # Round steps to integers and ensure no negative values
    mutate(
        step_count = pmax(0, round(step_count))
    ) %>%
    # Remove unnecessary columns
    select(user_id, date, step_count) %>%
    # Sort by user and date
    arrange(user_id, date)

