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
ggsave("boxplot_wind.png", p2,
    width = 6, height = 5, dpi = 300
)
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
ggsave("boxplot_temperature.png", p3,
    width = 6, height = 5, dpi = 300
)
