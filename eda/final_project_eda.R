# Load necessary libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)
library(tseries)
library(corrplot)

# ------------------------------
# 1. Read in the dataset
# ------------------------------
file_path <- "~/Downloads/cyber_data.csv"
cyber_data <- read.csv(file_path, stringsAsFactors = FALSE)
cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y")

# ------------------------------
# 2. Handle Missing Values
# ------------------------------
numeric_vars <- c("Spam", "Ransomware", "Local.Infection", "Exploit", 
                  "Malicious.Mail", "Network.Attack", "On.Demand.Scan", "Web.Threat")
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
ts_data <- ts(daily_data[,-1], start = c(2022, 10, 11), frequency = 365)

# ------------------------------
# 5. STL Decomposition (Trend, Seasonality, Residuals)
# ------------------------------
par(mfrow = c(3, 3))
for (col in colnames(ts_data)) {
  ts_series <- ts_data[, col]
  ts_series[is.na(ts_series)] <- mean(ts_series, na.rm = TRUE)
  decomposed <- stl(ts(ts_series, frequency = 30), s.window = "periodic")
  plot(decomposed, main = paste("STL Decomposition of", col))
}

# ------------------------------
# 6. Autocorrelation & Partial Autocorrelation (ACF/PACF)
# ------------------------------
par(mfrow = c(2, 4))  # Arrange in 2 rows, 4 columns
for (col in colnames(ts_data)) {
  ts_series <- ts_data[, col]
  acf(ts_series, main = paste("ACF of", col))
  pacf(ts_series, main = paste("PACF of", col))
}

# ------------------------------
# 7. Cross-Correlation (CCF)
# ------------------------------
par(mfrow = c(1, 2))
ccf(ts_data[, "Spam"], ts_data[, "Malicious.Mail"], lag.max = 30, 
    main = "CCF: Malicious Mail -> Spam")  

ccf(ts_data[, "Ransomware"], ts_data[, "Network.Attack"], lag.max = 30, 
    main = "CCF: Network Attack -> Ransomware")  

# ------------------------------
# 8. STL Decomposition Network Attack
# ------------------------------
daily_data <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(Network.Attack = mean(Network.Attack, na.rm = TRUE))

ts_network <- ts(daily_data$Network.Attack, start = c(2022, 10), frequency = 30)

# STL Decomposition
network_stl <- stl(ts_network, s.window = "periodic")
plot(network_stl, main = "STL Decomposition of Network.Attack")

# ------------------------------
# Modeling Network Attack
# ------------------------------

train_size <- floor(0.8 * length(ts_network))
train_data <- ts_network[1:train_size]
test_data  <- ts_network[(train_size + 1):length(ts_network)]

adf_test <- adf.test(train_data)
if (adf_test$p.value > 0.05) {
  train_data <- diff(train_data, differences = 1) 
  test_data <- diff(test_data, differences = 1)
}

arima_model <- auto.arima(train_data, seasonal = FALSE)
arima_forecast <- forecast(arima_model, h = length(test_data))

sarima_model <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
sarima_forecast <- forecast(sarima_model, h = length(test_data))

# Compute RMSE for SARIMA
sarima_rmse <- sqrt(mean((sarima_forecast$mean - test_data)^2, na.rm = TRUE))
cat("SARIMA RMSE:", sarima_rmse, "\n")

# ------------------------------
# Plot ARIMA vs SARIMA Forecast
# ------------------------------
start_date <- as.Date("2022-10-11")
full_dates <- seq(from = start_date, by = "days", length.out = length(ts_network))

ts_network_zoo <- zoo(ts_network, order.by = full_dates)
forecast_start <- full_dates[train_size + 1]
forecast_dates <- seq(from = forecast_start, by = "days", length.out = length(test_data))

test_zoo <- zoo(test_data, order.by = forecast_dates)
arima_zoo <- zoo(arima_forecast$mean, order.by = forecast_dates)
sarima_zoo <- zoo(sarima_forecast$mean, order.by = forecast_dates)

# Plot the forecasts
ggplot() + 
  geom_line(aes(x = index(ts_network_zoo), y = coredata(ts_network_zoo)), color = "gray", alpha = 0.5) +
  geom_line(aes(x = index(test_zoo), y = coredata(test_zoo)), color = "black", linetype = "solid", size = 0.25) +
  geom_line(aes(x = index(arima_zoo), y = coredata(arima_zoo)), color = "red", linetype = "solid", size = 0.5) +
  geom_line(aes(x = index(sarima_zoo), y = coredata(sarima_zoo)), color = "blue", linetype = "solid", size = 0.5) +
  ggtitle("ARIMA vs SARIMA Forecast") + 
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "3 months") + 
  theme_minimal() +
  labs(x = "Date", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------
# Checkresiduals
#-------------------------------

# Check residuals for ARIMA
checkresiduals(arima_model)

# Check residuals for SARIMA
checkresiduals(sarima_model)


