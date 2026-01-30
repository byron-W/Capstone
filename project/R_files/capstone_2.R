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
