

# 1. Exploratory Analysis

## 1.1 How many users are there?
user_daily_steps_final  %>%  distinct(user_id) %>% nrow()

## 1.1 User Demographics 

univariate_eda(user_daily_steps_final, "age_group")
univariate_eda(user_daily_steps_final, "sex")
population_density_plot <- univariate_eda(user_daily_steps_final  %>% select(user_id, population_density)  %>% distinct(), "population_density")



## 2.3 Daily Steps

x <- univariate_eda(user_daily_steps_final, "step_count")
x[[3]]

str(user_daily_steps_final)

sum(is.na(user_daily_steps_final$month))
sum(is.na(user_daily_steps_final$step_count))

## 2.2 Bi-Variate Analysis

# Sex & Step Count

bivariate_sex  <- bivariate_eda(user_daily_steps_final, "sex", "step_count")
bivariate_sex[[1]]

bivariate_age_group  <- bivariate_eda(user_daily_steps_final, "age_group", "step_count")
bivariate_age_group[[1]]
# Age-Group & Sex Count
x <- bivariate_eda(user_daily_steps_final, "month", "step_count")
x[[1]]

bivariate_eda(user_daily_steps_final, "weekdays", "step_count")

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

  