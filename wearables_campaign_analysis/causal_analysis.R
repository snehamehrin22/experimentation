# 1. Descriptive Analysis

fact_daily_treatment_group_filtered  %>% 
group_by(treatment_group) %>% 
summarize(mean_step_count = mean(step_count, na.rm = TRUE),
median_step_count = median(step_count, na.rm = TRUE),
sd_steps = sd(step_count, na.rm = TRUE),
n_observations = n()


)

# Box plot
ggplot(fact_daily_treatment_group_filtered, 
       aes(x = treatment_group, y = step_count)) +
  geom_boxplot() +
  labs(title = "Step Count Distribution by Treatment Period",
       x = "Treatment Period",
       y = "Daily Step Count") +
  theme_minimal()


  # Check original distribution
p1 <- ggplot(fact_daily_treatment_group_filtered, aes(x = step_count)) +
  geom_histogram(bins = 50) +
  labs(title = "Original Step Count Distribution",
       x = "Step Count",
       y = "Frequency") +
  theme_minimal()

# Check log-transformed distribution
p2 <- ggplot(fact_daily_treatment_group_filtered, aes(x = log(step_count + 1))) +
  geom_histogram(bins = 50) +
  labs(title = "Log-Transformed Step Count Distribution",
       x = "Log(Step Count + 1)",
       y = "Frequency") +
  theme_minimal()

# Display both plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# Quick normality test
shapiro.test(sample(fact_daily_treatment_group_filtered$step_count, 5000))  # Sample because Shapiro-Wilk is limited to 5000 observations
shapiro.test(sample(log(fact_daily_treatment_group_filtered$step_count + 1), 5000))

# 1. Initial Data Transformation
fact_daily_treatment_group_filtered <- fact_daily_treatment_group_filtered %>%
  mutate(log_steps = log(step_count + 1))  # Adding 1 to handle zero steps

# 2. Basic Linear Model
basic_model <- lm(log_steps ~ treatment_group + days_from_start, 
                 data = fact_daily_treatment_group_filtered)

# 3. Assumption Testing

# Linearity Check
ggplot(fact_daily_treatment_group_filtered, 
       aes(x = days_from_start, y = log_steps, color = treatment_group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(~treatment_group) +
  labs(title = "Linearity Check",
       x = "Days from Treatment Boundary",
       y = "Log Step Count") +
  theme_minimal()

  qqnorm(residuals(basic_model))
qqline(residuals(basic_model))


hist(autocorr_check$autocorr,
     main = "Distribution of Autocorrelation by User",
     xlab = "Autocorrelation")

# 4. Mixed Effects Model (accounting for user-level random effects)
library(lme4)
mixed_model <- lmer(log_steps ~ treatment_group + days_from_start + 
                   (user_id), 
                   data = fact_daily_treatment_group_filtered)

# Model summary
summary(mixed_model)

# 5. Robust Standard Errors
library(sandwich)
library(lmtest)

# Calculate robust standard errors
robust_se <- coeftest(basic_model, 
                     vcov = vcovHC(basic_model, type = "HC1"))

# 6. Results Visualization with Confidence Intervals
library(ggeffects)

# Plot predicted values
pred_plot <- ggpredict(mixed_model, terms = c("treatment_group"))
plot(pred_plot) +
  labs(title = "Predicted Step Counts by Treatment Group",
       x = "Treatment Group",
       y = "Log Step Count") +
  theme_minimal()

# 7. Summary Statistics
model_summary <- data.frame(
  statistic = c("AIC", "BIC", "Log-likelihood"),
  value = c(AIC(mixed_model), BIC(mixed_model), logLik(mixed_model))
)

print("Model Diagnostics:")
print(model_summary)
print("\nRobust Standard Errors:")
print(robust_se)