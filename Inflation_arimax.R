
library(forecast)
library(dplyr)
library(lubridate)
library(tseries)


# Set locale to English (for weekdays and month names)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Get the path of the currently open R script
current_script_path <- rstudioapi::getActiveDocumentContext()$path

# Extract the directory of the script
current_script_dir <- dirname(current_script_path)

ufv_file_path <- paste0(current_script_dir , "/UFV data.csv")

# UFV data
ufv_data <- read.csv(ufv_file_path)
ufv_data$Converted_Date <- dmy(ufv_data$Fecha, locale = "es_ES.UTF-8")
ufv_data <- ufv_data %>%
  rename(date = Converted_Date)

# USDT Data
usdt_file_path <- paste0(current_script_dir , "/USDT data.csv")
usdt_data <- read.csv(usdt_file_path)
usdt_data$date <- as.Date(usdt_data$category, format = "%a %b %d %Y")


# Merge UFV and USDT data on the date column
merged_data <- ufv_data %>%
  left_join(usdt_data, by = "date") %>% 
  select(date, UFV = Valor.de.la.UFV, USDT = Bs.)

# Check for missing values
sum(is.na(merged_data))  # Handle missing values if necessary
merged_data <- na.omit(merged_data)  # Remove rows with missing values

# Check for missing values
sum(is.na(merged_data))  # Handle missing values if necessary
merged_data <- na.omit(merged_data)  #


# Create an inflation indicator (e.g., ratio of UFV to USDT)
merged_data$inflation <- merged_data$UFV / merged_data$USDT

# Log-transform the inflation indicator to stabilize variance
merged_data$log_inflation <- log(merged_data$inflation)


# Augmented Dickey-Fuller test for stationarity
adf.test(merged_data$log_inflation)  # p > 0.05 → non-stationary

# If non-stationary, take first differences
merged_data$d_log_inflation <- c(NA, diff(merged_data$log_inflation))
merged_data <- na.omit(merged_data)  # Remove NA from differencing

# Define target variable (y) and external regressors (xreg)
y <- ts(merged_data$d_log_inflation, frequency = 12)  # Differenced log inflation
xreg <- merged_data$USDT  # External regressor (USDT)



# Split into training and test sets (optional)
train_size <- floor(0.8 * nrow(merged_data))
train_y <- y[1:train_size]
train_xreg <- xreg[1:train_size]

# Fit ARIMAX model
model_arimax <- auto.arima(
  train_y,
  xreg = train_xreg,
  seasonal = TRUE,
  stepwise = TRUE
)

# Print model summary
summary(model_arimax)


# Prepare future xreg data (e.g., USDT values for the test set)
future_xreg <- xreg[(train_size + 1):nrow(merged_data)]

# Forecast future inflation
forecast_arimax <- forecast(model_arimax, xreg = future_xreg)

# Plot results
plot(forecast_arimax, main = "ARIMAX Forecast of Inflation")



# Residual diagnostics
checkresiduals(model_arimax)


# Compare forecasted vs actual values (if test set exists)
test_y <- y[(train_size + 1):nrow(merged_data)]
accuracy(forecast_arimax, test_y)


plot(forecast_arimax, main = "ARIMAX Forecast of Inflation")
lines(test_y, col = "red")  # Add actual test values
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)



# Extract point forecasts (mean)
forecast_values <- forecast_arimax$mean

# Extract prediction intervals (if available)
lower_interval <- forecast_arimax$lower[, "95%"]  # 95% lower bound
upper_interval <- forecast_arimax$upper[, "95%"]  # 95% upper bound


# Create a dataframe with forecasted values
forecast_table <- data.frame(
  date = seq.Date(
    from = max(merged_data$date) + 1,  # Start from the day after the last date
    by = "month",                      # Adjust frequency as needed
    length.out = length(forecast_values)
  ),
  forecast_mean = as.numeric(forecast_values),  # Point forecasts
  forecast_lower = as.numeric(lower_interval),  # Lower bound
  forecast_upper = as.numeric(upper_interval)   # Upper bound
)

# View the forecast table
print(forecast_table)


# Historical table
historical_table <- data.frame(
  date = merged_data$date,
  actual_value = merged_data$inflation,  # Use the original target variable
  forecast_mean = NA,  # No forecasts for historical data
  forecast_lower = NA,  # No lower bounds for historical data
  forecast_upper = NA   # No upper bounds for historical data
)
# Forecast table
forecast_table <- data.frame(
  date = seq.Date(
    from = max(merged_data$date) + 1,  # Start from the day after the last date
    by = "month",                      # Adjust frequency as needed
    length.out = length(forecast_values)
  ),
  actual_value = NA,  # No actual values for future dates
  forecast_mean = as.numeric(forecast_values),  # Point forecasts
  forecast_lower = as.numeric(lower_interval),  # Lower bound
  forecast_upper = as.numeric(upper_interval)   # Upper bound
)

# Combine historical and forecasted data
combined_table <- rbind(historical_table, forecast_table)

# Save to CSV
write.csv(combined_table, file = "combined_forecast.csv", row.names = FALSE)
