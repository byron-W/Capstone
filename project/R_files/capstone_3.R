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
            subtitle = paste(
                "r =",
                round(cor(df[[var]], df$Download.Speed), 3)
            )
        )
    weather_scatter_plots[[var]] <- p
}
# Arrange and save
weather_grid <- do.call(grid.arrange, c(weather_scatter_plots, ncol = 3))
ggsave("weather_scatterplots.png", weather_grid,
    width = 12, height = 8, dpi = 300
)
# ======================================
# SPLIT DATA
# ======================================
set.seed(123)
train_index <- createDataPartition(df$Download.Speed,
    p = 0.8, list = FALSE
)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

cat("\nTraining set:", nrow(train_data), "observations")
cat("\nTest set:", nrow(test_data), "observations\n")
