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
