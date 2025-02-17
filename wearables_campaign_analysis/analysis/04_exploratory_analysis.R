# ===================================
# Exploratory Data Analysis of Step Count Data
# Purpose: Analyze user walking patterns and demographic relationships
# ===================================

# Load required packages (if not already loaded)
# library(tidyverse)
# library(tidyquant)  # for theme_tq()

# ===================================
# 1. Basic User Analysis
# ===================================

# Count unique users in dataset
n_users <- user_daily_steps_final %>% 
    distinct(user_id) %>% 
    nrow()

# Analyze demographic distributions
# Age distribution
age_dist <- univariate_eda(user_daily_steps_final, "age_group")
# Gender distribution
sex_dist <- univariate_eda(user_daily_steps_final, "sex")

# ===================================
# 2. Step Count Analysis
# ===================================

# Overall step count distribution
step_dist <- univariate_eda(user_daily_steps_final, "step_count")
step_dist[[3]]  # Plot the distribution

# ===================================
# 3. Demographic Impact Analysis
# ===================================

# Analyze step counts by gender
bivariate_sex <- bivariate_eda(user_daily_steps_final, "sex", "step_count")
bivariate_sex[[1]]

# Analyze step counts by age group
bivariate_age_group <- bivariate_eda(user_daily_steps_final, "age_group", "step_count")
bivariate_age_group[[2]]

# ===================================
# 4. Temporal Analysis
# ===================================

# Campaign impact analysis
bivariate_step_count <- bivariate_eda(step_count_final, "period", "step_count")
bivariate_step_count[[1]]

# Weekend vs. Weekday analysis
bivariate_eda(step_count_final, "weekdays", "step_count")

# Seasonal analysis
bivariate_eda(step_count_final, "month", "step_count")

# ===================================
# 5. Multi-factor Analysis
# ===================================

# Calculate summary statistics for age-gender combinations
age_gender_summary <- step_count_final %>%
    group_by(sex, age_group) %>%
    summarise(
        mean_steps = mean(step_count, na.rm = TRUE),
        median_steps = median(step_count, na.rm = TRUE),
        n_observations = n(),
        n_users = n_distinct(user_id)
    ) %>%
    ungroup() %>%
    arrange(desc(mean_steps))

# Visualize age-gender interaction
ggplot(step_count_final, aes(x = age_group, y = step_count, fill = sex)) +
    geom_boxplot(alpha = 0.5) +
    theme_tq() +
    labs(
        title = "Step Count Distribution by Age Group and Sex",
        x = "Age Group",
        y = "Daily Step Count"
    ) +
    scale_fill_manual(values = c("female" = "#FF9999", "male" = "#66B2FF")) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )





