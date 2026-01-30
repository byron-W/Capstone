library(dplyr)
library(ggplot2)
library(corrplot)
library(lsr) # for Cohen's d
library(gridExtra) # for arranging plots
library(reshape2)
library(ggcorrplot)
library(caret)

# ======================================
# DATA PREPARATION
# ======================================

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
