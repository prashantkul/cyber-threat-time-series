# Load necessary libraries
library(ggplot2)
library(dplyr)
library(zoo)         # For handling missing values
library(forecast)    # For time series decomposition
library(tseries)     # For ACF/PACF analysis
library(corrplot)    # For correlation heatmaps

# ------------------------------
# 1. Read in the dataset
# ------------------------------
file_path <- "~/Downloads/cyber_data.csv"  # Adjust path if needed
cyber_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Convert the AttackDate column to Date format
cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y")

# ------------------------------
# 2. Handle Missing Values
# ------------------------------
# Select numeric attack columns
numeric_vars <- c("Spam", "Ransomware", "Local.Infection", "Exploit", 
                  "Malicious.Mail", "Network.Attack", "On.Demand.Scan", "Web.Threat")

# Fill NAs with linear interpolation (or use other methods if preferred)
cyber_data[numeric_vars] <- lapply(cyber_data[numeric_vars], function(x) na.approx(x, rule = 2))

# ------------------------------
# 3. Aggregate Daily Means
# ------------------------------
daily_data <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(across(all_of(numeric_vars), mean, na.rm = TRUE))

# ------------------------------
# 4. Convert to Time Series
# ------------------------------
ts_data <- ts(daily_data[,-1], start = c(2022, 10), frequency = 30)  # Assuming daily data

# ------------------------------
# 5. STL Decomposition (Trend, Seasonality, Residuals)
# ------------------------------
par(mfrow = c(3, 3))  # Arrange plots in a grid
for (col in colnames(ts_data)) {
  ts_series <- ts_data[, col]
  ts_series[is.na(ts_series)] <- mean(ts_series, na.rm = TRUE)  # Fill any remaining NAs
  decomposed <- stl(ts(ts_series, frequency = 30), s.window = "periodic")
  plot(decomposed, main = paste("STL Decomposition of", col))
}

# Overall Behavior: The Web Threat time series exhibits a clear seasonal pattern, a fluctuating trend, and significant noise in the remainder component.
# Seasonality: The second panel highlights strong periodic fluctuations, indicating that Web Threat activity follows a recurring cycle—possibly daily or weekly patterns.
# Trend: The third panel shows a long-term variation in Web Threat occurrences, with an initial decline, a stabilization phase, and a recent upward trend. This suggests shifts in attack intensity over time.
# Residuals (Remainder): The bottom panel shows high-frequency noise, meaning there are unpredictable short-term variations not captured by the trend or seasonality.
# Implications: The strong seasonal component suggests that Web Threat activity is not purely random but follows a predictable cycle. This insight could be useful for proactive threat mitigation strategies. The increasing trend at the end might indicate a growing risk, requiring closer monitoring.

# ------------------------------
# 6. Autocorrelation & Partial Autocorrelation (ACF/PACF)
# ------------------------------
par(mfrow = c(2, 4))  # Arrange in 2 rows, 4 columns
for (col in colnames(ts_data)) {
  ts_series <- ts_data[, col]
  acf(ts_series, main = paste("ACF of", col))   # AutoCorrelation
  pacf(ts_series, main = paste("PACF of", col)) # Partial AutoCorrelation
}

# ------------------------------
# 7. Cross-Correlation (CCF) - Example Between Spam & Malicious Mail
# ------------------------------
par(mfrow = c(1, 2))  # Arrange 2 plots side by side
ccf(ts_data[, "Spam"], ts_data[, "Malicious.Mail"], lag.max = 30, 
    main = "CCF: Malicious Mail -> Spam")  

ccf(ts_data[, "Ransomware"], ts_data[, "Network.Attack"], lag.max = 30, 
    main = "CCF: Network Attack -> Ransomware")  


# ----------------------------------
# 8. STL Decompositon Network Attack
# -----------------------------------

cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y %H:%M")

# Aggregate daily means for Network Attack
daily_data <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(Network.Attack = mean(Network.Attack, na.rm = TRUE))

# Convert to time series
ts_network <- ts(daily_data$Network.Attack, start = c(2022, 10), frequency = 30)  # Assuming daily data

# STL Decomposition
network_stl <- stl(ts_network, s.window = "periodic")

# Plot the decomposition
plot(network_stl, main = "STL Decomposition of Network.Attack")
