
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)
library(tseries)
library(lubridate)
library(Metrics)

# ------------------------------
# 1. Read & Prepare Data
# ------------------------------
file_path <- "~/Downloads/cyber_data.csv"
cyber_data <- read.csv(file_path, stringsAsFactors = FALSE)

cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y")

usa_malicious <- cyber_data %>%
  filter(Country == "United States of America") %>%
  select(AttackDate, Malicious.Mail)

usa_malicious$Malicious.Mail <- na.approx(usa_malicious$Malicious.Mail, rule = 2)

weekly_data <- usa_malicious %>%
  mutate(Week = floor_date(AttackDate, "week")) %>%
  group_by(Week) %>%
  summarise(Malicious.Mail = mean(Malicious.Mail, na.rm = TRUE)) %>%
  ungroup()

weekly_ts <- ts(weekly_data$Malicious.Mail, frequency = 52,
                start = c(year(min(weekly_data$Week)), week(min(weekly_data$Week))))

# ------------------------------
# 2. Check Stationarity (NO Differencing Applied)
# ------------------------------
adf_result <- adf.test(weekly_ts)
cat("\nADF Test p-value:", adf_result$p.value, "\n")
if (adf_result$p.value > 0.05) {
  cat("\nWARNING: Time series is non-stationary, but differencing is NOT applied.\n")
}

# ------------------------------
# 3. Train-Test Split
# ------------------------------
train_size <- floor(0.8 * length(weekly_ts))
train_data <- weekly_ts[1:train_size]
test_data  <- weekly_ts[(train_size + 1):length(weekly_ts)]
test_dates <- weekly_data$Week[(train_size + 1):length(weekly_ts)]

# ------------------------------
# 4. Model 1: ARIMA
# ------------------------------
arima_model <- auto.arima(train_data)
arima_forecast <- forecast(arima_model, h = length(test_data))
arima_preds <- arima_forecast$mean
arima_rmse <- rmse(test_data, arima_preds)

summary(arima_model)

# ------------------------------
# 5. Model 2: ETS (Exponential Smoothing)
# ------------------------------
ets_model <- ets(train_data)
ets_forecast <- forecast(ets_model, h = length(test_data))
ets_preds <- ets_forecast$mean
ets_rmse <- rmse(test_data, ets_preds)


ets_model
# ------------------------------
# 6. Model 3: ARMA
# ------------------------------
arma_model <- Arima(train_data, order = c(2, 0, 2))  # ARMA(2,2)
arma_forecast <- forecast(arma_model, h = length(test_data))
arma_preds <- arma_forecast$mean
arma_rmse <- rmse(test_data, arma_preds)

summary(arma_model)

# ------------------------------
# 7. Compare RMSEs
# ------------------------------
rmse_results <- data.frame(
  Model = c("ARIMA", "ETS", "ARMA"),
  RMSE  = c(arima_rmse, ets_rmse, arma_rmse)
)
print(rmse_results)

# ------------------------------
# 8. Train Data vs Model Predictions
# ------------------------------
train_dates <- weekly_data$Week[1:train_size]

arima_fitted <- fitted(arima_model)
ets_fitted <- fitted(ets_model)
arma_fitted <- fitted(arma_model)

train_plot_df <- data.frame(
  Date = train_dates,
  Train = train_data,
  ARIMA = arima_fitted,
  ETS = ets_fitted,
  ARMA = arma_fitted
)

ggplot(train_plot_df, aes(x = Date)) +
  geom_line(aes(y = Train, color = "Train Data"), size = 1, alpha = 0.5) +
  geom_line(aes(y = ARIMA, color = "ARIMA"), size = 1) +
  geom_line(aes(y = ETS, color = "ETS"), size = 1) +
  geom_line(aes(y = ARMA, color = "ARMA"), size = 1) +
  labs(title = "Model Predictions vs Train Data",
       y = "Malicious Mail Attacks",
       x = "Date",
       color = "Legend") +
  scale_color_manual(values = c("Train Data" = "black",
                                "ARIMA" = "red",
                                "ETS" = "blue",
                                "ARMA" = "green")) +
  theme_minimal()

# ------------------------------
# 9. Model Predictions vs Test Data (Attach to Train)
# ------------------------------
full_dates <- c(train_dates, test_dates)
actual_full <- c(train_data, test_data)

arima_full <- c(arima_fitted, rep(NA, length(test_data)))
ets_full <- c(ets_fitted, rep(NA, length(test_data)))
arma_full <- c(arma_fitted, rep(NA, length(test_data)))

arima_full[(train_size + 1):length(actual_full)] <- arima_preds
ets_full[(train_size + 1):length(actual_full)] <- ets_preds
arma_full[(train_size + 1):length(actual_full)] <- arma_preds

plot_df <- data.frame(
  Date = full_dates,
  Train = c(actual_full[1:train_size], rep(NA, length(test_data))),
  Test = c(rep(NA, train_size), actual_full[(train_size + 1):length(actual_full)]),
  ARIMA = arima_full,
  ETS = ets_full,
  ARMA = arma_full
)

# Plot with fixed color mapping
ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Train, color = "Train Data"), size = 1) +
  geom_line(aes(y = Test, color = "Test Data"), size = 1) +
  geom_line(aes(y = ARIMA, color = "ARIMA"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = ETS, color = "ETS"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = ARMA, color = "ARMA"), size = 0.5, linetype = "solid") +
  labs(title = "Model Forecast vs Test Data (Attached to Train)",
       y = "Malicious Mail Attacks",
       x = "Date",
       color = "Legend") +
  scale_color_manual(values = c("Train Data" = "gray",
                                "Test Data" = "gray",
                                "ARIMA" = "red",
                                "ETS" = "blue",
                                "ARMA" = "green")) +
  theme_minimal()
# ------------------------------
# 10. Residual Analysis
# ------------------------------
arima_residuals <- residuals(arima_model)
ets_residuals <- residuals(ets_model)
arma_residuals <- residuals(arma_model)

# Histograms for Residual Normality
par(mfrow = c(3, 1))
hist(arima_residuals, main = "ARIMA Residuals", col = "red", breaks = 20)
hist(ets_residuals, main = "ETS Residuals", col = "blue", breaks = 20)
hist(arma_residuals, main = "ARMA Residuals", col = "green", breaks = 20)
par(mfrow = c(1, 1))

# ------------------------------
# 11. ACF & PACF Plots
# ------------------------------
par(mfrow = c(3, 2))
acf(arima_residuals, main = "ARIMA ACF")
pacf(arima_residuals, main = "ARIMA PACF")
acf(ets_residuals, main = "ETS ACF")
pacf(ets_residuals, main = "ETS PACF")
acf(arma_residuals, main = "ARMA ACF")
pacf(arma_residuals, main = "ARMA PACF")
par(mfrow = c(1, 1))


# ------------------------------
# 1. Fit ETS Model
# ------------------------------
ets_model <- ets(train_data)
ets_forecast <- forecast(ets_model, h = length(test_data))

# ------------------------------
# 2. Fit ARMA on ETS Residuals
# ------------------------------
ets_residuals <- residuals(ets_model)
arma_residuals_model <- auto.arima(ets_residuals, seasonal = FALSE)
arma_residuals_forecast <- forecast(arma_residuals_model, h = length(test_data))

# ------------------------------
# 3. Combine ETS + ARMA Forecasts
# ------------------------------
improved_forecast <- ets_forecast$mean + arma_residuals_forecast$mean

# ------------------------------
# 4. Prepare Data for Plotting
# ------------------------------
full_dates <- c(train_dates, test_dates)

plot_df <- data.frame(
  Date = full_dates,
  Train = c(train_data, rep(NA, length(test_data))),
  Test = c(rep(NA, length(train_data)), test_data),
  ETS = c(fitted(ets_model), ets_forecast$mean),
  ETS_ARMA = c(fitted(ets_model), improved_forecast)
)

# ------------------------------
# 5. Plot ETS vs ETS+ARMA Forecasts
# ------------------------------
ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Train, color = "Train Data"), size = 1) +
  geom_line(aes(y = Test, color = "Test Data"), size = 1) +
  geom_line(aes(y = ETS, color = "ETS Forecast"), size = 1, linetype = "dashed") +
  geom_line(aes(y = ETS_ARMA, color = "ETS + ARMA Forecast"), size = 1) +
  labs(title = "ETS vs ETS + ARMA Forecast",
       y = "Malicious Mail Attacks",
       x = "Date",
       color = "Legend") +
  scale_color_manual(values = c(
    "Train Data" = "black",
    "Test Data" = "gray",
    "ETS Forecast" = "blue",
    "ETS + ARMA Forecast" = "red"
  )) +
  theme_minimal()
