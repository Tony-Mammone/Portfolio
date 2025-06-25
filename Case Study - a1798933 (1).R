# Load required libraries
library(tidyverse)


### Data Preparation


# Read and prepare data
melbourne <- read_csv("melbourne.csv") %>%
  mutate(
    Date = as.Date(Date),
    Month = factor(month(Date), levels = 1:12, labels = month.abb),
    Day = factor(weekdays(Date)),
    Evaporation = `Evaporation (mm)`,
    MaxTemp = `Maximum Temperature (Deg C)`,
    MinTemp = `Minimum temperature (Deg C)`,
    Humidity9am = `9am relative humidity (%)`
  ) %>%
  select(Date, Month, Day, Evaporation, MaxTemp, MinTemp, Humidity9am)


### Bivariate Summaries


# 1. Month vs Evaporation (Categorical)
month_plot <- ggplot(melbourne, aes(x = Month, y = Evaporation)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Evaporation by Month",
       y = "Evaporation (mm)")

# 2. Day of Week vs Evaporation (Categorical)
day_plot <- ggplot(melbourne, aes(x = Day, y = Evaporation)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Evaporation by Day of Week",
       y = "Evaporation (mm)")

# 3. Maximum Temperature vs Evaporation (Continuous)
maxtemp_plot <- ggplot(melbourne, aes(x = MaxTemp, y = Evaporation)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Evaporation vs Maximum Temperature",
       x = "Maximum Temperature (°C)",
       y = "Evaporation (mm)")

# 4. Minimum Temperature vs Evaporation (Continuous)
mintemp_plot <- ggplot(melbourne, aes(x = MinTemp, y = Evaporation)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Evaporation vs Minimum Temperature",
       x = "Minimum Temperature (°C)",
       y = "Evaporation (mm)")

# 5. 9am Humidity vs Evaporation (Continuous)
humidity_plot <- ggplot(melbourne, aes(x = Humidity9am, y = Evaporation)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Evaporation vs 9am Humidity",
       x = "Relative Humidity at 9am (%)",
       y = "Evaporation (mm)")


### Model Section


# 1. Initial full model with all predictors and interaction
full_model <- lm(Evaporation ~ Month + Day + MaxTemp + MinTemp + 
                   Humidity9am + Month:Humidity9am, data = melbourne)

# Print initial model summary and ANOVA
summary(full_model)
anova(full_model)

# 2. Remove least significant term (based on highest p-value)
model1 <- update(full_model, . ~ . - Month:Humidity9am)
summary(model1)
anova(model1)

# 3. Continue removing terms until all are significant at 5% level
model2 <- update(model1, . ~ . - Day)
summary(model2)
anova(model2)

# Store final model
final_model <- model2


### Model Diagnostics & Testing


# 1. Linearity
linearity_plot <- ggplot(data.frame(
  fitted = fitted(final_model),
  residuals = residuals(final_model)
), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# 2. Normality
qq_plot <- ggplot(data.frame(
  std_resid = rstandard(final_model)
), aes(sample = std_resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Normal Q-Q Plot")

# 3. Homoscedasticity
scale_location_plot <- ggplot(data.frame(
  fitted = fitted(final_model),
  std_resid = sqrt(abs(rstandard(final_model)))
), aes(x = fitted, y = std_resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal() +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "√|Standardized Residuals|")

# 4. Independence
acf_plot <- acf(residuals(final_model), plot = FALSE)


### Predictions


# Create prediction data for specific dates
pred_data <- tibble(
  Month = factor(c("Feb", "Dec", "Jan", "Jul"), levels = month.abb),
  MaxTemp = c(23.2, 31.9, 44.3, 10.6),
  MinTemp = c(13.8, 16.4, 26.5, 6.8),
  Humidity9am = c(74, 57, 35, 76)
)

# Get predictions with both confidence and prediction intervals
predictions <- pred_data %>%
  mutate(
    # Point predictions
    fit = predict(final_model, newdata = ., interval = "none"),
    
    # Confidence intervals (95%)
    conf_int = predict(final_model, newdata = ., interval = "confidence", level = 0.95),
    conf_lwr = conf_int[,"lwr"],
    conf_upr = conf_int[,"upr"],
    
    # Prediction intervals (95%)
    pred_int = predict(final_model, newdata = ., interval = "prediction", level = 0.95),
    pred_lwr = pred_int[,"lwr"],
    pred_upr = pred_int[,"upr"],
    
    # 10mm threshold analysis
    status = case_when(
      pred_lwr > 10 ~ "Will exceed 10mm",
      pred_upr < 10 ~ "Will not exceed 10mm",
      TRUE ~ "Uncertain"
    )
  )

### Create results table
results_table <- predictions %>%
  select(
    Month,
    MaxTemp,
    MinTemp,
    Humidity9am,
    Predicted = fit,
    `Conf.Int.Lower` = conf_lwr,
    `Conf.Int.Upper` = conf_upr,
    `Pred.Int.Lower` = pred_lwr,
    `Pred.Int.Upper` = pred_upr,
    Status = status
  ) %>%
  mutate(across(where(is.numeric), round, 2))

print(results_table)

# Visualization of predictions with intervals
pred_plot <- ggplot(predictions, aes(x = Month, y = fit)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf_lwr, ymax = conf_upr), 
                width = 0.2, color = "red", size = 1) +
  geom_errorbar(aes(ymin = pred_lwr, ymax = pred_upr), 
                width = 0.4, color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Predicted Evaporation with 95% Confidence and Prediction Intervals",
       y = "Evaporation (mm)",
       caption = "Red bars: Confidence intervals for mean\nBlue bars: Prediction intervals\nDashed line: 10mm threshold")

### Display all plots & Results
print(month_plot)
print(day_plot)
print(maxtemp_plot)
print(mintemp_plot)
print(humidity_plot)
print(linearity_plot)
print(qq_plot)
print(scale_location_plot)
print(pred_plot)

summary(model1)
anova(model1)

summary(model2)
anova(model2)

print(results_table)
