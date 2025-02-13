
# 1. Data Quality Checks
initial_data_check(dim_user)

initial_data_check(fact_user_health_campaign)

initial_data_check(fact_daily_steps)

# 2. Exploratory Analysis

## 2.1 User Demographics 

univariate_eda(dim_user, "age_group")
univariate_eda(dim_user, "sex")
univariate_eda(dim_user, "population_density")


## 2.2 User Health Campaigns

user_health_campaign_cleaned  <-  fact_user_health_campaign  %>% 
left_join(dim_user, by = 'user_id')  %>% 
left_join(dim_health_campaign, by = 'campaign_id') 

univariate_eda(user_health_campaign_cleaned, "campaign_name")


## 2.3 Daily Steps

x <- univariate_eda(fact_daily_steps, "step_count")
x[[1]]


## 2.2 Bi-Variate Analysis

# Sex & Step Count

bivariate_eda(fact_daily_steps_cleaned, "sex", "step_count")

bivariate_eda(fact_daily_steps_cleaned, "age_group", "step_count")
# Age-Group & Sex Count
bivariate_eda(fact_daily_steps_cleaned, "month", "step_count")

bivariate_eda(fact_daily_steps_cleaned, "weekdays", "step_count")

### 2.3 Multi Variate Analysis

#### 2.3.2 Step Count Across Age-Group, Gender & Sex

library(ggplot2)

# Assuming your dataset is called `fact_daily_steps`
ggplot(fact_daily_steps_cleaned  %>%  filter(sex!='unknown'), aes(x = step_count, fill = sex)) +
  geom_density(alpha = 0.5) +  # Semi-transparent to see overlap
  facet_wrap(~ age_group) +  # Separate plots for each age group
  labs(title = "Density Plot of Step Counts by Sex & Age Group",
       x = "Step Count",
       y = "Density") +
  theme_minimal() + 
  scale_fill_manual(values = c("red", "blue", "green")

  