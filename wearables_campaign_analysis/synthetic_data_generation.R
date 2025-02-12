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
    # Join to get user activation date and campaign activities
    left_join(dim_user %>% select(user_id, activated_ts), by = "user_id") %>%
    left_join(dim_health_campaign %>% select(campaign_id, num_campaign_activities), by = "campaign_id") %>%
    mutate(
        campaign_enrolled_ts = pmap_dbl(list(activated_ts), function(act_date) {
            possible_dates <- seq(act_date, as_datetime("2024-01-01"), by = "day")
            sample(possible_dates, 1) %>% as.numeric()
        }) %>% as_datetime(),
        
        completion_status = sample(c("completed", "quit", "ongoing"), 
                                 n_fact_records, 
                                 prob = c(0.70, 0.20, 0.10), 
                                 replace = TRUE),
        
        # Now num_campaign_activities will be properly aligned with each record
        campaign_completed_ts = case_when(
            completion_status == "completed" ~ campaign_enrolled_ts + days(num_campaign_activities),
            TRUE ~ as.POSIXct(NA)
        ),
        
        campaign_quit_ts = case_when(
            completion_status == "quit" ~ campaign_enrolled_ts + days(sample(1:14, 1)),
            TRUE ~ as.POSIXct(NA)
        ) ) %>% 
    select(-activated_ts, -completion_status)




# Create daily step data

# Create daily step data
fact_daily_steps <- dim_user %>%
    # Start with user and their activation dates
    select(user_id, activated_ts) %>%
    # Create all possible user-date combinations
    crossing(date = seq(min(as_date(activated_ts)), ymd("2024-01-01"), by = "day")) %>%
    # Only keep dates after user activation
    filter(date >= as_date(activated_ts)) %>%
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
    # Round steps to integers and ensure no negative values
    mutate(
        step_count = pmax(0, round(step_count))
    ) %>%
    # Remove activation date column
    select(user_id, date, step_count) %>%
    # Sort by user and date
    arrange(user_id, date)

