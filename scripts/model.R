# Aggregate daily means for Local Infection & On-Demand Scan
daily_trends <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(
    LocalInfection = mean(Local.Infection, na.rm = TRUE),
    OnDemandScan   = mean(On.Demand.Scan, na.rm = TRUE),
    .groups = "drop"
  )

# Quick line plot
ggplot(daily_trends, aes(x = AttackDate)) +
  geom_line(aes(y = LocalInfection, color = "Local.Infection")) +
  geom_line(aes(y = OnDemandScan, color = "On.Demand.Scan")) +
  labs(title = "Local Infection vs. On-Demand Scan (Global Average)",
       x = "Date", y = "Attack Percentage") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Filter data for the USA
usa_data <- cyber_data %>%
  filter(Country == "United States of America")  # Adjust if the country name differs

# Compute daily averages
usa_daily <- usa_data %>%
  group_by(AttackDate) %>%
  summarise(
    LocalInfection = mean(Local.Infection, na.rm = TRUE),
    OnDemandScan   = mean(On.Demand.Scan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AttackDate)

# Plot both series
ggplot(usa_daily, aes(x = AttackDate)) +
  geom_line(aes(y = LocalInfection, color = "Local Infection")) +
  geom_line(aes(y = OnDemandScan, color = "On-Demand Scan")) +
  labs(
    title = "Local Infection vs. On-Demand Scan (USA)",
    x = "Date",
    y = "Attack Percentage"
  ) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Global daily
global_daily <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(
    LocalInfection = mean(Local.Infection, na.rm = TRUE),
    OnDemandScan   = mean(On.Demand.Scan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AttackDate)


# Let us find correation and cross-correlation between Local Infection and On-Demand Scan
cor(usa_daily$LocalInfection, usa_daily$OnDemandScan, use = "complete.obs")
cor(global_daily$LocalInfection, global_daily$OnDemandScan, use = "complete.obs")

cat("When you average across a large set of countries, short-term fluctuations or local anomalies tend to cancel out, leading to smoother, 
    more consistent time-series. This can make two globally aggregated metrics appear more correlated because they both follow an overall global trend.
    The USA might have more localized spikes or idiosyncratic events that cause Local Infection and On-Demand Scan to deviate at times, lowering the correlation compared to the global pattern.
	  Different security postures, different timing of patch deployments, or varying threat vectors could introduce additional “noise” in one country versus the global mean.")

# step-by-step guide for a deeper time-series analysis of Local Infection and On-Demand Scan in the USA
usa_data <- cyber_data %>%
  filter(Country == "United States of America") %>%
  group_by(AttackDate) %>%
  summarise(
    LocalInfection = mean(Local.Infection, na.rm = TRUE),
    OnDemandScan   = mean(On.Demand.Scan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AttackDate)

head(usa_data)

# Identify the first date to set a "start" for your ts
start_date <- min(usa_data$AttackDate)
local_ts <- ts(usa_data$LocalInfection,
               start = c(year(start_date), yday(start_date)),
               frequency = 365)

ondemand_ts <- ts(usa_data$OnDemandScan,
                  start = c(year(start_date), yday(start_date)),
                  frequency = 365)



# Linear interpolation for NA values in local_ts
local_ts_imputed <- na.approx(local_ts)

length(local_ts_imputed)

#7-day repeating pattern rather than a 365-day pattern.
local_ts_imputed_7 <- ts(local_ts_imputed, frequency = 7)

decomp_local_7 <- stl(local_ts_imputed_7, s.window = "periodic")
plot(decomp_local_7, main = "STL with Weekly Frequency - LocalInfection")

remainder_local <- decomp_local_7$time.series[, "remainder"]

# ACF plot
Acf(remainder_local, main = "ACF of STL Remainder - Local Infection")

Box.test(remainder_local, lag = 14, type = "Ljung-Box")

hist(remainder_local, breaks = 30, main = "Histogram of STL Remainder")

print("Remainder still has autocorrealtion, we will try to remove it by differencing")

remainder_7 <- decomp_local_7$time.series[, "remainder"]

# Differencing the remainder
remainder_diff_7 <- diff(remainder_7, lag = 1)  # first difference
Acf(remainder_diff_7, main = "ACF of Differenced Remainder")
Box.test(remainder_diff_7, lag = 14, type = "Ljung-Box")

#Construct the Seasonally Adjusted Series

sa_series_7 <- local_ts_imputed_7 - decomp_local_7$time.series[, "seasonal"]

# Fit an ARIMA model

fit_arima_7 <- auto.arima(sa_series_7)
summary(fit_arima_7)


checkresiduals(fit_arima_7)

residuals_arima <- residuals(fit_arima_7)


Box.test(residuals_arima, lag = 14, type = "Ljung-Box")


forecasted <- forecast(fit_arima_7, h = 14)  # e.g., 14-day forecast
autoplot(forecasted)

# save the plot
ggsave(filename = "output/plots/forecasted_local_infection.png")

### Compare with actual

# Suppose usa_data has columns: AttackDate, LocalInfection
n <- nrow(usa_data)
train_size <- n - 14  # last 14 days as test

train_data <- usa_data[1:train_size, ]  # up to day n-14
test_data  <- usa_data[(train_size + 1):n, ]  # final 14 days

train_ts <- ts(train_data$LocalInfection,
               start = c(year(min(train_data$AttackDate)), 
                         yday(min(train_data$AttackDate))),
               frequency = 7)

fit_train <- auto.arima(train_ts, seasonal = TRUE)
summary(fit_train)

# Forecast into the test period
h <- nrow(test_data)  # 14
fcast <- forecast(fit_train, h = h)
# The forecasted values:
fcast_vals <- as.data.frame(fcast)$`Point Forecast`

# The actual test data:
actual_vals <- test_data$LocalInfection

# Compare side by side
comparison_df <- data.frame(
  Date      = test_data$AttackDate,
  Forecast  = fcast_vals,
  Actual    = actual_vals
)
comparison_df

#1) Create a combined data frame
# We'll also create an index to place the forecast in time
forecast_dates <- seq.Date(
  from = max(train_data$AttackDate) + 1,
  by   = "day",
  length.out = h
)

plot_df <- data.frame(
  Date     = c(train_data$AttackDate, forecast_dates),
  Value    = c(train_data$LocalInfection, fcast_vals),
  DataType = c(rep("Train", nrow(train_data)), rep("Forecast", h))
)

# Add actual test data for comparison
test_plot_df <- data.frame(
  Date     = test_data$AttackDate,
  Value    = test_data$LocalInfection,
  DataType = "Actual"
)

# Combine them
plot_all <- rbind(plot_df, test_plot_df)

# 2) Plot
ggplot(plot_all, aes(x = Date, y = Value, color = DataType)) +
  geom_line() +
  labs(title = "ARIMA Forecast vs. Actual",
       x = "Date", y = "Local Infection %") +
  theme_minimal()

# Save the plot
ggsave(filename = "output/plots/arima_forecast_vs_actual.png")