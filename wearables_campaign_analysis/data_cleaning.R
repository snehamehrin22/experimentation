
source('synthetic_data_generation.R')

dim_user %>% 
summarize(min_date = min(activated_ts),
max_date = max(activated_ts)
)

dim_health_campaign


fact_user_health_campaign  %>% 
group_by(campaign_id)  %>% 
summarize(total_users = n(),
unique_users = n_distinct(user_id)
)

# users enrolled in multiple campaigns
fact_user_health_campaign  %>% 
group_by(user_id, campaign_id)  %>% 
summarize(total = n())


# checking if users enrolled in multiple campaigns if there is overlap in between dates

users_enrolled_in_multiple_campaigns  <- fact_user_health_campaign  %>% 
group_by(user_id,campaign_id) %>% 
summarize(total_campaigns = n()) %>% 
filter(total_campaigns>1)  %>% 
ungroup()

users_enrolled_in_multiple_campaigns_check  <- 
fact_user_health_campaign  %>% inner_join(users_enrolled_in_multiple_campaigns, by = c('user_id','campaign_id'))  

users_enrolled_in_multiple_campaigns_check  %>% arrange(user_id, campaign_id)  %>% view()
# checking if campaign_enrolled_ts > activated_ts
fact_user_health_campaign  %>% 
left_join(dim_user, by = 'user_id')  %>% 
filter(campaign_enrolled_ts<activated_ts)


# checking if campaign_completed_ts is not null, then campaign_quit_ts is null
fact_user_health_campaign  %>% 
filter(!is.na(campaign_completed_ts),!is.na(campaign_quit_ts))

# checking if campaign_completed_ts > activated_ts

fact_user_health_campaign  %>% 
left_join(dim_user, by = 'user_id')  %>% 
filter(campaign_completed_ts<activated_ts)

# Checking Min & Max Of Campaign Enrolled Dates

fact_user_health_campaign  %>% 
summarise(min_date = max(campaign_enrolled_ts),min_completed_ts = min(campaign_completed_ts),
min_quit_ts = min(campaign_quit_ts)
)

# fact_daily_steps cleaning

fact_daily_steps_cleaned  <- fact_daily_steps  %>% 
left_join(dim_user, by = 'user_id')  %>% 
mutate(month = month(date)  %>% as_factor(),

weekdays = weekdays(date)  %>%  as_factor()
)


# Missing Data Imputation In Fact Daily Steps

steps_continuity_analysis  <-  fact_daily_steps_cleaned  %>% 
group_by(user_id)  %>% 
summarize(total_days = n(),
first_date = min(date),
last_date = max(date),
expected_days = as.numeric(difftime(last_date, first_date, units = 'days'))+ 1,
completeness = (total_days/expected_days)*100

)  %>% 
ungroup()



steps_continuity_analysis


# Summary statistics
steps_continuity_analysis %>%
  summarize(
    avg_completeness = mean(completeness),
    median_completeness = median(completeness),
    users_above_90pct = sum(completeness >= 90),
    total_users = n(),
    min_completeness = min(completeness),
    max_completeness = max(completeness)
  )

  hist(as.numeric(steps_continuity_analysis$completeness), 
     main = "Distribution of Data Completeness by User",
     xlab = "Completeness (%)",
     breaks = 20)



# Checking how many users have missing days less than 7 days

fact_daily_steps_cleaned  %>% 
group_by(user_id)  %>% 
mutate(missing_days = as.numeric(date - lag(date)) ) %>%  
ungroup() %>% 
select(user_id, missing_days)  %>% 
filter(missing_days>1 & missing_days<7)  %>% 
distinct(user_id)  %>% 
nrow()

# Only 7 % has l
# So we do moving average to impute the missing data

# 1. First identify users with acceptable gaps and enough data
users_with_small_gaps <- fact_daily_steps_cleaned %>%
  arrange(user_id, date) %>%
  group_by(user_id) %>%
  mutate(
    days_gap = as.numeric(date - lag(date)),
    max_gap = if(all(is.na(days_gap))) {
      0  # If user has only one record
    } else {
      max(days_gap, na.rm = TRUE)
    }
  ) %>%
  filter(max_gap <= 7) %>%
  filter(sum(!is.na(step_count)) >= 2) %>%
  pull(user_id) %>%
  unique()

# 2. Filter dataset and apply moving average
fact_daily_steps_cleaned_imputed <- fact_daily_steps_cleaned %>%
  filter(user_id %in% users_with_small_gaps) %>%
  arrange(user_id, date) %>%
  group_by(user_id) %>%
  summarize(start_date = min(date),
  end_date = max(date)
  
  )  %>% 
  ungroup ()  %>% 
group_by (user_id)  %>% 
expand(date = seq.Date(start_date, end_date, by = 'day'))  %>% 
ungroup() %>% 
left_join(fact_daily_steps_cleaned, by = c('user_id','date')) %>% 
mutate(step_count = case_when(
    is.na(step_count) ~ zoo::rollmean(step_count, k = 7, fill = "extend", align = "center"),
    TRUE ~ step_count
    )
    
    )  %>%  
select(-age_group, - sex, -population_density, -activated_ts)  %>% 
left_join(dim_user, by = 'user_id')  %>% 
mutate(month = month(date)  %>% as_factor(),
weekdays = weekdays(date)  %>%  as_factor()
)



fact_daily_treatment_group  <- fact_daily_steps_cleaned_imputed  %>% 
left_join(fact_user_health_campaign  %>% 
group_by (user_id) %>% 
summarize(first_enrolled_date = as.Date(min(campaign_enrolled_ts)),
last_completed_date = as.Date(max(campaign_completed_ts))
)  %>% 
ungroup(), by = 'user_id')  %>% 
mutate(treatment_group = case_when(
  date< first_enrolled_date ~ 'pre-treatment-period',
  date> last_completed_date~ 'post-treatment-period',
  TRUE ~ 'treatment-period'
))



fact_daily_treatment_group_filtered <- fact_daily_treatment_group %>%
  group_by(user_id, treatment_group) %>%
  arrange(user_id, date) %>%
  mutate(
    days_from_start = case_when(
      treatment_group == "pre-treatment-period" ~ as.numeric(first_enrolled_date - date),
      treatment_group == "post-treatment-period" ~ as.numeric(date - last_completed_date),
      TRUE ~ 0
    )
  ) %>%
  ungroup()  %>% 
  filter(days_from_start<=60, treatment_group != 'treatment-period') 



