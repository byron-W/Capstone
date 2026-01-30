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
comparison_df <- data.frame(
    Model = c("Weather-Only", "Time-Based", "Combined"),
    Train_R2 = c(weather_train_r2, time_train_r2, combined_train_r2),
    Test_R2 = c(weather_test_r2, time_test_r2, combined_test_r2),
    Train_RMSE = c(weather_train_rmse, time_train_rmse, combined_train_rmse),
    Test_RMSE = c(weather_test_rmse, time_test_rmse, combined_test_rmse)
)

comparison_long <- comparison_df[, c("Model", "Test_R2")]
names(comparison_long)[2] <- "R²"

comparison_plot <- ggplot(comparison_long, aes(x = Model, y = `R²`)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = round(`R²`, 3)), vjust = -0.5) +
    theme_minimal() +
    labs(
        title = "Model Performance Comparison (Test Set)",
        y = "R² (Coefficient of Determination)"
    ) +
    ylim(0, max(comparison_long$`R²`) * 1.2)

ggsave("model_comparison.png", comparison_plot,
    width = 8, height = 6, dpi = 300
)
