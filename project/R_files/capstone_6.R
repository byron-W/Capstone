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
    geom_abline(
        intercept = 0, slope = 1, linetype = "dashed",
        color = "red"
    ) +
    theme_minimal() +
    labs(
        title = "Actual vs Predicted: Combined Model",
        x = "Actual Download Speed (Mbps)",
        y = "Predicted Download Speed (Mbps)"
    )
ggsave("actual_vs_predicted_combined.png", plot_combined,
    width = 6, height = 6, dpi = 300
)
