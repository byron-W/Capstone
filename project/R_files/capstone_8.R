# ======================================
# EXTREME WEATHER ANALYSIS
# ======================================
# Create extreme weather indicators
df <- df %>%
    mutate(
        extreme_precip = ifelse(One.Hour.Precipitation
        > quantile(One.Hour.Precipitation, 0.9),
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
    mean(df$Download.Speed[df$extreme_precip == "Heavy Precipitation"]),
    "Mbps\n"
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
ggsave("boxplot_precip.png", p1,
    width = 6, height = 5, dpi = 300
)
