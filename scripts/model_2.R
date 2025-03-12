decomp_local_usa <- stl(local_ts_imputed_7, s.window = "periodic")
plot(decomp_local_usa, main = "STL Decomposition of Local Infection (USA)")

# Aggregate daily data for USA
usa_daily <- cyber_data %>%
  filter(Country == "United States of America") %>%
  group_by(AttackDate) %>%
  summarise(
    LocalInfection = mean(Local.Infection, na.rm = TRUE),
    OnDemandScan = mean(On.Demand.Scan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AttackDate)

# Step A: Model Local Infection separately using SARIMA.
# Impute missing values
usa_daily$LocalInfection <- na.approx(usa_daily$LocalInfection)
usa_daily$OnDemandScan <- na.approx(usa_daily$OnDemandScan)

# Define weekly seasonal time series
local_ts_usa <- ts(usa_daily$LocalInfection, frequency = 7)
scan_ts_usa  <- ts(usa_daily$OnDemandScan, frequency = 7)

# Fit SARIMA Model (Local Infection only)
# Automatically choose SARIMA parameters
sarima_local <- auto.arima(local_ts_usa, seasonal = TRUE)

# Model summary
summary(sarima_local)

checkresiduals(sarima_local)
Box.test(residuals(sarima_local), lag = 14, type = "Ljung-Box")

sarimax_local_scan <- auto.arima(local_ts_usa, xreg = scan_ts_usa, seasonal = TRUE)
checkresiduals(sarimax_local_scan)
Box.test(residuals(sarimax_local_scan), lag = 14, type = "Ljung-Box")

# Still residual autocorrelation, trying TBATS

# Fit TBATS Model (Local Infection only)
tbats_fit <- tbats(local_ts_usa)

summary(tbats_fit)

checkresiduals(tbats_fit)

# still not capturing the seasonality, trying prophet

#############################################################
# Prophet
#############################################################

# Prepare data
df_prophet <- data.frame(ds = usa_daily$AttackDate, y = usa_daily$LocalInfection)
df_prophet$ds <- as.Date(df_prophet$ds, format = "%d/%m/%Y %H:%M")

# Fit
m <- prophet(df_prophet, daily.seasonality=TRUE)

# Create future dataframe matching the training period
# (Alternatively, you can just re-use your original dates)
future <- df_prophet[, "ds", drop = FALSE]

# Predict on the same (in-sample) dates to get fitted values
forecast_in_sample <- predict(m, future)

# Plot
plot(m, forecast_in_sample)
prophet_plot_components(m, forecast_in_sample)


res_df <- merge(
  forecast_in_sample[, c("ds", "yhat")],
  df_prophet[, c("ds", "y")],
  by = "ds"
)

res_df$residual <- res_df$y - res_df$yhat

res_ts <- ts(res_df$residual, frequency = 7)  # if daily data, weekly freq

Box.test(res_ts, lag = 14, type = "Ljung-Box")

# Prophet for Network Attack
# Prepare data

usa_daily_nw <- cyber_data %>%
  filter(Country == "United States of America") %>%
  group_by(AttackDate) %>%
  summarise(
    NetworkAttack = mean(Network.Attack, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AttackDate)

# Step A: Model Local Infection separately using SARIMA.
# Impute missing values
usa_daily_nw$NetworkAttack <- na.approx(usa_daily_nw$NetworkAttack)

df_prophet_network <- data.frame(ds = usa_daily_nw$AttackDate, y = usa_daily_nw$NetworkAttack)

df_prophet_network$ds <- as.Date(df_prophet_network$ds, format = "%d/%m/%Y %H:%M")

# Fit
# Fit
m_2 <- prophet(df_prophet_network, daily.seasonality=TRUE)

# Create future dataframe matching the training period
# (Alternatively, you can just re-use your original dates)
future_2 <- df_prophet_network[, "ds", drop = FALSE]

# Predict on the same (in-sample) dates to get fitted values
forecast_in_sample_2 <- predict(m_2, future_2)

# Plot
plot(m_2, forecast_in_sample_2)
prophet_plot_components(m_2, forecast_in_sample_2)


res_df_2 <- merge(
  forecast_in_sample_2[, c("ds", "yhat")],
  df_prophet_network[, c("ds", "y")],
  by = "ds"
)

# Ljung box test and rmse for Prophet model




res_df_2$residual <- res_df_2$y - res_df_2$yhat

res_ts_2 <- ts(res_df_2$residual, frequency = 7)  # if daily data, weekly freq

Box.test(res_ts_2, lag = 14, type = "Ljung-Box")

rmse <- sqrt(mean(res_df_2^2, na.rm = TRUE))

Box.test(res_df_2$residual, lag = 10, type = "Ljung") 


# Add the Regressor to the Prophet Model
df_prophet_2 <- data.frame(ds = usa_daily$AttackDate, y = usa_daily$LocalInfection, scan = usa_daily$OnDemandScan)

df_prophet_2$ds <- as.Date(df_prophet_2$ds, format = "%d/%m/%Y %H:%M")


# Fit the model
m_2 <- prophet(df_prophet_2, daily.seasonality=TRUE)

# Create future dataframe matching the training period
future_2 <- df_prophet_2[, "ds", drop = FALSE]

# Predict on the same (in-sample) dates to get fitted values
forecast_in_sample_2 <- predict(m_2, future_2)

# Plot
plot(m_2, forecast_in_sample_2)
prophet_plot_components(m_2, forecast_in_sample_2)


#############################################################
# Neural Network
#############################################################

nnar_model <- nnetar(local_ts_imputed_7)
summary(nnar_model)
checkresiduals(nnar_model)

# Forecast
nnar_forecast <- forecast(nnar_model, h = 30)
plot(nnar_forecast)

# WHat are we forecasting?
nnar_forecast


# Let us forecast Network Attack
# Example: if 'cyber_data' has daily rows, with a column 'Network.Attack'
network_daily <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(NetworkAttack = mean(Network.Attack, na.rm = TRUE), .groups = "drop") %>%
  arrange(AttackDate)

# Impute any NAs
network_daily$NetworkAttack <- na.approx(network_daily$NetworkAttack)

# Convert to time series (assuming daily data with weekly seasonality => frequency=7)
network_ts <- ts(network_daily$NetworkAttack, frequency = 7)

# Plot the time series
plot(network_ts, main = "Network Attack Time Series", xlab = "Time", ylab = "Network Attack")

nnar_model <- nnetar(network_ts)
summary(nnar_model)
checkresiduals(nnar_model)

# Check the time range
tsp(network_ts)
# Suppose this returns something like c(1, 59.86, 7)

n <- length(network_ts)  # total number of observations
train_end <- (n - 14) / frequency(network_ts) + start(network_ts)[1] - 1
# The above logic ensures we end 14 points before the last observation.

train_ts <- window(network_ts, end = train_end)
test_ts  <- window(network_ts, start = train_end + 1/frequency(network_ts))
# Fit on training portion
nnar_train <- nnetar(train_ts)
nnar_forecast_train <- forecast(nnar_train, h = 14)

# Compare predictions to actual
accuracy(nnar_forecast_train, test_ts)

# Plot Forecast
plot(nnar_forecast_train, main = "Network Attack Forecast", xlab = "Time", ylab = "Network Attack")

# Try L2 regularization

y <- ts(train_ts, frequency = 7)

# y is your univariate time series
fit_nnetar <- nnetar(
  y,
  p = 14,         # number of lagged inputs
  size = 10,      # hidden neurons
  repeats = 20,   # ensemble
  decay = 0.001,  # weight decay for regularization
  maxit = 1000    # more training iterations
)

summary(fit_nnetar)
checkresiduals(fit_nnetar)

# let use train_Ts and test_Ts that we already defined above
forecast_nnetar <- forecast(fit_nnetar, h = 14)
plot(forecast_nnetar, main = "Network Attack Forecast", xlab = "Time", ylab = "Network Attack")

# Compare predictions to actual
accuracy(forecast_nnetar, test_ts)


