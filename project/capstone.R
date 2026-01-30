library(dplyr)
library(ggplot2)
library(corrplot)
library(lsr) # for Cohen's d
library(gridExtra) # for arranging plots
library(reshape2)
library(ggcorrplot)
library(caret)

# load data
df <- read.csv("capstone_data.csv")

# Remove index column
df$X <- NULL

# Check missing values as proportions
print(round(colSums(is.na(df)) / nrow(df), 4))

# Drop specific columns
df$Heat.Index <- NULL
df$Wind.Chill <- NULL
df$Gust <- NULL

# Drop rows with missing values in specific columns
df <- df |>
  filter(!is.na(Temperature)) |>
  filter(!is.na(Dew.Point)) |>
  filter(!is.na(Relative.Humidity)) |>
  filter(!is.na(One.Hour.Precipitation)) |>
  filter(!is.na(Wind.Speed)) |>
  filter(!is.na(Direction)) |>
  filter(!is.na(Visibility))

# Check missing values again
print(round(colSums(is.na(df)) / nrow(df), 4))

# ======================================
# CORRELATION ANALYSIS
# ======================================

# Create correlation matrix
cor_matrix <- cor(df |> select_if(is.numeric))

# Extract correlations with Download Speed
download_cors <- cor_matrix[, "Download.Speed"]
download_cors <- download_cors[names(download_cors) != "Download.Speed"]
download_cors <- sort(abs(download_cors), decreasing = TRUE)

cat("\nAbsolute Correlations with Download Speed (sorted):\n")
print(round(download_cors, 4))

# Dropping features
df$Dew.Point <- NULL
df$Direction <- NULL
df$Test.Count <- NULL

# Visualize correlation matrix
other_vars <- setdiff(colnames(cor_matrix), "Download.Speed")
new_order <- c("Download.Speed", other_vars)

cor_matrix_reordered <- cor_matrix[new_order, new_order]
a <- ggcorrplot(cor_matrix_reordered, lab = TRUE)
ggsave("correlation_matrix.png", plot = a)

# ======================================
# FEATURE ANALYSIS
# ======================================

weather_scatter_plots <- list()

weather_vars <- c(
  "Temperature", "Relative.Humidity",
  "One.Hour.Precipitation", "Wind.Speed", "Visibility"
)

for (var in weather_vars) {
  p <- ggplot(df, aes_string(x = var, y = "Download.Speed")) +
    geom_point(alpha = 0.2, color = "steelblue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    theme_minimal() +
    labs(
      title = paste(var, "vs Download Speed"),
      subtitle = paste("r =", round(cor(df[[var]], df$Download.Speed), 3))
    )
  weather_scatter_plots[[var]] <- p
}

# Arrange and save
weather_grid <- do.call(grid.arrange, c(weather_scatter_plots, ncol = 3))
ggsave("weather_scatterplots.png", weather_grid, width = 12, height = 8, dpi = 300)

# ======================================
# SPLIT DATA
# ======================================

set.seed(123)

train_index <- createDataPartition(df$Download.Speed, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

cat("\nTraining set:", nrow(train_data), "observations")
cat("\nTest set:", nrow(test_data), "observations\n")

# ======================================
# WEATHER-ONLY MODEL
# ======================================

model_weather <- lm(
  Download.Speed ~ Temperature +
    Relative.Humidity + One.Hour.Precipitation +
    Wind.Speed + Visibility,
  data = train_data
)

summary(model_weather)

# Predictions and metrics
pred_weather_train <- predict(model_weather, train_data)
pred_weather_test <- predict(model_weather, test_data)

weather_train_r2 <- cor(train_data$Download.Speed, pred_weather_train)^2
weather_test_r2 <- cor(test_data$Download.Speed, pred_weather_test)^2

cat("Training R²:", round(weather_train_r2, 4), "\n")
cat("Test R²:", round(weather_test_r2, 4), "\n")

results_weather <- data.frame(
  Actual = test_data$Download.Speed,
  Predicted = pred_weather_test,
  Model = "Weather-Only"
)

plot_weather <- ggplot(results_weather, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Actual vs Predicted: Weather-Only Model",
    x = "Actual Download Speed (Mbps)",
    y = "Predicted Download Speed (Mbps)"
  )
ggsave("actual_vs_predicted_weather.png", plot_weather, width = 6, height = 6, dpi = 300)

# ======================================
# TIME-BASED MODEL
# ======================================

model_time <- lm(Download.Speed ~ Hour, data = train_data)
summary(model_time)

# Predictions and metrics
pred_time_train <- predict(model_time, train_data)
pred_time_test <- predict(model_time, test_data)

time_train_r2 <- cor(train_data$Download.Speed, pred_time_train)^2
time_test_r2 <- cor(test_data$Download.Speed, pred_time_test)^2

cat("Training R²:", round(time_train_r2, 4), "\n")
cat("Test R²:", round(time_test_r2, 4), "\n")

results_time <- data.frame(
  Actual = test_data$Download.Speed,
  Predicted = pred_time_test,
  Model = "Time-Based"
)

plot_time <- ggplot(results_time, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Actual vs Predicted: Time-Based Model",
    x = "Actual Download Speed (Mbps)",
    y = "Predicted Download Speed (Mbps)"
  )
ggsave("actual_vs_predicted_time.png", plot_time, width = 6, height = 6, dpi = 300)

# ======================================
# COMBINED MODEL
# ======================================

model_combined <- lm(
  Download.Speed ~ Hour + Temperature +
    Relative.Humidity + One.Hour.Precipitation +
    Wind.Speed + Visibility,
  data = train_data
)

summary(model_combined)

# Predictions and metrics
pred_combined_train <- predict(model_combined, train_data)
pred_combined_test <- predict(model_combined, test_data)

combined_train_r2 <- cor(train_data$Download.Speed, pred_combined_train)^2
combined_test_r2 <- cor(test_data$Download.Speed, pred_combined_test)^2

cat("Training R²:", round(combined_train_r2, 4), "\n")
cat("Test R²:", round(combined_test_r2, 4), "\n")

results_combined <- data.frame(
  Actual = test_data$Download.Speed,
  Predicted = pred_combined_test,
  Model = "Combined"
)

plot_combined <- ggplot(results_combined, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Actual vs Predicted: Combined Model",
    x = "Actual Download Speed (Mbps)",
    y = "Predicted Download Speed (Mbps)"
  )
ggsave("actual_vs_predicted_combined.png", plot_combined, width = 6, height = 6, dpi = 300)

# Residual plot
results_combined$Residuals <- results_combined$Actual - results_combined$Predicted

residual_plot <- ggplot(results_combined, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Residual Plot (Combined Model)",
    x = "Predicted Download Speed (Mbps)",
    y = "Residuals"
  )

ggsave("residual_plot.png", residual_plot, width = 8, height = 6, dpi = 300)

# Model comparison bar chart
# comparison_df <- data.frame(
#   Model = c("Weather-Only", "Time-Based", "Combined"),
#   Train_R2 = c(weather_train_r2, time_train_r2, combined_train_r2),
#   Test_R2 = c(weather_test_r2, time_test_r2, combined_test_r2),
# )
# 
# comparison_long <- comparison_df[, c("Model", "Test_R2")]
# names(comparison_long)[2] <- "R²"
# 
# comparison_plot <- ggplot(comparison_long, aes(x = Model, y = `R²`)) +
#   geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
#   geom_text(aes(label = round(`R²`, 3)), vjust = -0.5) +
#   theme_minimal() +
#   labs(
#     title = "Model Performance Comparison (Test Set)",
#     y = "R² (Coefficient of Determination)"
#   ) +
#   ylim(0, max(comparison_long$`R²`) * 1.2)
# 
# ggsave("model_comparison.png", comparison_plot, width = 8, height = 6, dpi = 300)
# ======================================
# EXTREME WEATHER ANALYSIS
# ======================================

# Create extreme weather indicators
df <- df %>%
  mutate(
    extreme_precip = ifelse(One.Hour.Precipitation > quantile(One.Hour.Precipitation, 0.9),
      "Heavy Precipitation", "Normal"
    ),
    extreme_temp = case_when(
      Temperature < quantile(Temperature, 0.1) ~ "Very Cold",
      Temperature > quantile(Temperature, 0.9) ~ "Very Hot",
      TRUE ~ "Normal"
    ),
    high_wind = ifelse(Wind.Speed > quantile(Wind.Speed, 0.9),
      "High Wind", "Normal"
    )
  )

# ======================================
# PRECIPITATION ANALYSIS
# ======================================
precip_ttest <- t.test(Download.Speed ~ extreme_precip, data = df)
print(precip_ttest)
cat(
  "\nMean download speed (Normal):",
  mean(df$Download.Speed[df$extreme_precip == "Normal"]), "Mbps\n"
)
cat(
  "Mean download speed (Heavy Precipitation):",
  mean(df$Download.Speed[df$extreme_precip == "Heavy Precipitation"]), "Mbps\n"
)
precip_cohens_d <- cohensD(Download.Speed ~ extreme_precip, data = df)
cat("Cohen's d (effect size):", round(precip_cohens_d, 4), "\n")
p1 <- ggplot(df, aes(x = extreme_precip, y = Download.Speed)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Download Speed: Normal vs Heavy Precipitation",
    x = "Condition",
    y = "Download Speed (Mbps)"
  )
ggsave("boxplot_precip.png", p1, width = 6, height = 5, dpi = 300)

# ======================================
# WIND ANALYSIS
# ======================================
wind_ttest <- t.test(Download.Speed ~ high_wind, data = df)
print(wind_ttest)
cat(
  "\nMean download speed (Normal):",
  mean(df$Download.Speed[df$high_wind == "Normal"]), "Mbps\n"
)
cat(
  "Mean download speed (High Wind):",
  mean(df$Download.Speed[df$high_wind == "High Wind"]), "Mbps\n"
)
wind_cohens_d <- cohensD(Download.Speed ~ high_wind, data = df)
cat("Cohen's d (effect size):", round(wind_cohens_d, 4), "\n")

p2 <- ggplot(df, aes(x = high_wind, y = Download.Speed)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Download Speed: Normal vs High Wind",
    x = "Condition",
    y = "Download Speed (Mbps)"
  )
ggsave("boxplot_wind.png", p2, width = 6, height = 5, dpi = 300)

# ======================================
# TEMPERATURE ANALYSIS
# ======================================
temp_aov <- aov(Download.Speed ~ extreme_temp, data = df)
print(summary(temp_aov))
TukeyHSD(temp_aov)
ss_total <- sum((df$Download.Speed - mean(df$Download.Speed))^2)
ss_temp <- 699121 # From ANOVA output
eta_sq <- ss_temp / ss_total
cat("Eta-squared: ", round(eta_sq, 4), "\n")

p3 <- ggplot(df, aes(x = extreme_temp, y = Download.Speed)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Download Speed by Temperature Condition",
    x = "Temperature Condition",
    y = "Download Speed (Mbps)"
  )
ggsave("boxplot_temperature.png", p3, width = 6, height = 5, dpi = 300)
