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
    geom_abline(
        intercept = 0, slope = 1, linetype = "dashed",
        color = "red"
    ) +
    theme_minimal() +
    labs(
        title = "Actual vs Predicted: Weather-Only Model",
        x = "Actual Download Speed (Mbps)",
        y = "Predicted Download Speed (Mbps)"
    )
ggsave("actual_vs_predicted_weather.png", plot_weather,
    width = 6, height = 6, dpi = 300
)
