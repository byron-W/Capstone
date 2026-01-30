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
    geom_abline(
        intercept = 0, slope = 1, linetype = "dashed",
        color = "red"
    ) +
    theme_minimal() +
    labs(
        title = "Actual vs Predicted: Time-Based Model",
        x = "Actual Download Speed (Mbps)",
        y = "Predicted Download Speed (Mbps)"
    )
ggsave("actual_vs_predicted_time.png", plot_time,
    width = 6, height = 6, dpi = 300
)
