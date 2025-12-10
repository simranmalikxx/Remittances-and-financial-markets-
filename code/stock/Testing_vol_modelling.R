
install.packages("readxl")
install.packages("FinTS")
install.packages("rugarch")
library(readxl)
library(dplyr)
library(forecast)
library(FinTS)
library(rugarch)
library(tseries)

data <- readxl::read_excel("E:/sem 5/econo/proj/Remittances-and-financial-markets/fin_data/full_dataset/dataset.xlsx")

# Quick check
head(data)
str(data)


# 1.
#Take the log of the index
data$KSE_log <- log(data$KSE100)

#Compute log returns as difference from previous period
data$KSE_ret <- data$KSE_log - dplyr::lag(data$KSE_log)

#We model the logarithmic returns of the KSE-100 index rather than the levels because asset returns are typically stationary, while index levels are not. Volatility modelling is also defined in terms of returns.



#2 .
#plotting the log returns 

# Convert 'year' and 'month' to a proper Date format (e.g., "2000-01-01")
month_to_num <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                  "Jul" = 7, "Aug" = 8, "Sept" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
data$month_num <- month_to_num[data$month]
data$Date <- as.Date(paste(data$year, data$month_num, "01", sep="-"))
head(data$Date)

# Plot the KSE returns with Date on the x-axis
plot(data$Date, data$KSE_ret, type = "l", 
     main = "KSE-100 Returns", ylab = "Return", xlab = "Date", 
     col = "blue", lwd = 2)

#The KSE-100 returns plot shows clear volatility clustering, where large movements are followed by more large movements, and calm periods follow other calm periods. This indicates time-varying volatility and suggests that ARMA and GARCH models are needed. The returns appear stationary, but we now need to check for autocorrelation to determine if an ARMA model is required for the mean. If autocorrelation is found, we will proceed with ARMA for mean modeling and GARCH for volatility modeling.



#3. Stationarity Test
adf_test <- adf.test(na.omit(data$KSE_ret), alternative = "stationary")
print(adf_test)




# 4. ACF for returns
acf(na.omit(data$KSE_ret), main = "ACF of KSE Returns")
#The ACF of KSE-100 Returns plot shows no significant autocorrelation in the returns, indicating that the mean process is white noise and doesn't require an ARMA model. However, since we observed volatility clustering earlier, we now need to focus on modeling volatility. The next step is to check for volatility clustering by examining the ACF of squared returns, which will help determine if GARCH models are necessary for capturing time-varying volatility.




# 5.Test for autocorrelation in returns
Box.test(data$KSE_ret, lag = 12, type = "Ljung")
#The Box-Ljung test on KSE-100 returns shows a p-value of 0.7599, indicating no significant autocorrelation in the returns. This means the returns are independent and don't follow any predictable pattern based on past returns. As a result, ARMA models for the mean are not needed. The focus now shifts to modeling volatility, and we will proceed by checking the ACF of squared returns to assess if volatility clustering exists, which would justify the use of GARCH models.



# 6. acf for squared returns
acf(na.omit(data$KSE_ret^2), main = "ACF of Squared KSE Returns")
#The ACF of Squared KSE Returns shows weak volatility clustering, with a few significant spikes at early lags, but it decays quickly. This suggests mild time-varying volatility, making a GARCH(1,1) model suitable. However, we should complement the plots with ARCH LM tests to confirm the presence of volatility clustering before proceeding with GARCH modeling.




# 7. Test for ARCH effects (conditional heteroskedasticity)
library(FinTS)
ArchTest(data$KSE_ret, lags = 12)
#The ARCH LM test resulted in a p-value of 0.1673, suggesting no significant ARCH effects in the KSE-100 returns. This implies that there is no evidence of time-varying volatility or volatility clustering, making GARCH models unnecessary. The next step is to check the ACF of squared returns to confirm volatility behavior and decide if we should proceed with simpler volatility models or focus on other aspects of the analysis.



# 8.Trying simlpler volatility models : Rolling Vol
library(zoo)

# Compute rolling standard deviation (volatility) over a 12-month window
data$rolling_volatility <- rollapply(data$KSE_ret, width = 12, FUN = sd, fill = NA, align = "right")

# Plot rolling volatility
plot(data$Date, data$rolling_volatility, type = "l", main = "Rolling Volatility of KSE Returns", ylab = "Volatility")
#The rolling volatility plot of KSE-100 returns reveals intermittent volatility spikes but no persistent volatility clustering, indicating that time-varying volatility exists without the need for GARCH models. Based on the evidence from the ARCH LM test and ACF of squared returns, simpler models like EWMA or rolling variance are sufficient for modeling volatility.



# 9. EWMA
# Set the smoothing parameter
lambda <- 0.94
ewma <- rep(NA, length(data$KSE_ret))
ewma[1] <- data$KSE_ret[2]^2  # Starting with the first non-NA squared return

# Compute the rest of the EWMA values
for (t in 2:length(ewma)) {
  ewma[t] <- lambda * ewma[t-1] + (1 - lambda) * data$KSE_ret[t]^2
}

# Add the calculated EWMA volatility to the data frame
data$ewma_vol <- sqrt(ewma)

# Plot the EWMA volatility of KSE returns
plot(data$Date, data$ewma_vol, type = "l", 
     main = "EWMA Volatility of KSE Returns", 
     ylab = "Volatility", col = "blue", lwd = 2)



# 10. GARCH (MEH - doesnt work)

returns <- na.omit(data$KSE_ret)  # Remove any NA values

# Step 1: Specify the GARCH(1,1) model
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),  # No ARMA needed
  distribution.model = "norm"  # Assuming normal errors for simplicity, you can try "std" for student t distribution
)

# Step 2: Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = returns)

# Step 3: Summarize the fit
summary(garch_fit)

cond_volatility <- sigma(garch_fit)

dates_cond_vol <- data$Date[-1]  # Remove the first date (it doesn't have a conditional volatility value)

# Plot the conditional volatility
plot(dates_cond_vol, cond_volatility, type = "l", 
     main = "Conditional Volatility (GARCH(1,1)) of KSE Returns", 
     ylab = "Volatility", col = "blue", lwd = 2)



# 11. Forecasting garch 

# Split the data into training and testing sets
train_size <- floor(0.8 * length(data$KSE_ret))  # 80% for training, 20% for testing
train_data <- data$KSE_ret[1:train_size]
test_data <- data$KSE_ret[(train_size + 1):length(data$KSE_ret)]

# Remove missing values (NA) from training data
train_data <- na.omit(train_data)

# Fit the GARCH(1,1) model on the training data
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")

# Fit the model to the training data (without NA values)
garch_fit <- ugarchfit(spec = garch_spec, data = train_data)


# Forecast the conditional volatility (forecast future volatility)
garch_forecast <- ugarchforecast(garch_fit, n.ahead = length(test_data))  # Forecast for the test period

# Extract the forecasted volatility (sigma values)
forecasted_volatility <- sigma(garch_forecast)

# Plot the forecasted volatility vs actual volatility (from squared returns)
# Prepare the test period dates
test_dates <- data$Date[(train_size + 1):length(data$Date)]

# Actual volatility: using squared returns as a proxy for volatility
actual_volatility <- sqrt(test_data^2)

# Plot both forecasted and actual volatility for comparison
plot(test_dates, forecasted_volatility, type = "l", col = "red", lwd = 2, 
     main = "Volatility Forecasting: GARCH(1,1) vs Actual Volatility", 
     ylab = "Volatility", xlab = "Date")
lines(test_dates, actual_volatility, col = "blue", lwd = 2)
legend("topright", legend = c("Forecasted Volatility (GARCH)", "Actual Volatility"), 
       col = c("red", "blue"), lwd = 2)






# 12.

# Calculate rolling volatility for the training period (12-month window)
rolling_vol_train <- rollapply(train_data, width = 12, FUN = sd, fill = NA, align = "right")

# Now, forecast rolling volatility for the test period using the same window
rolling_vol_forecast <- rep(NA, length(test_data))

for (i in 1:length(test_data)) {
  # Combine the last 12 training returns with the test data to create a rolling window
  window_data <- c(train_data[(length(train_data) - 11):length(train_data)], test_data[1:i])
  rolling_vol_forecast[i] <- sd(window_data)
}

# Plot rolling volatility vs actual volatility
plot(test_dates, rolling_vol_forecast, type = "l", col = "red", lwd = 2, 
     main = "Rolling Volatility Forecasting vs Actual Volatility", 
     ylab = "Volatility", xlab = "Date")

# Plot actual volatility (from squared returns)
lines(test_dates, sqrt(test_data^2), col = "green", lwd = 2)
legend("topright", legend = c("Rolling Volatility Forecast", "Actual Volatility"), 
       col = c("red", "green"), lwd = 2)






# 13.

# Set the smoothing parameter (lambda), typically between 0.9 and 0.94
lambda <- 0.94

# Create a vector to store the EWMA volatility values for the training data
ewma_vol_train <- rep(NA, length(train_data))

# Initializing the first value (starting with the first squared return)
ewma_vol_train[1] <- train_data[2]^2  # The first value should be squared return of the second point

# Compute the rest of the EWMA values for the training data
for (t in 2:length(ewma_vol_train)) {
  ewma_vol_train[t] <- lambda * ewma_vol_train[t - 1] + (1 - lambda) * train_data[t]^2
}

# Now, forecast for the test period (continue the EWMA calculation for the test data)
ewma_vol_forecast <- rep(NA, length(test_data))
ewma_vol_forecast[1] <- lambda * ewma_vol_train[length(ewma_vol_train)] + (1 - lambda) * test_data[1]^2

for (t in 2:length(ewma_vol_forecast)) {
  ewma_vol_forecast[t] <- lambda * ewma_vol_forecast[t - 1] + (1 - lambda) * test_data[t]^2
}

# Plot the forecasted EWMA volatility vs actual volatility
plot(test_dates, sqrt(ewma_vol_forecast), type = "l", col = "blue", lwd = 2,
     main = "EWMA Volatility Forecasting vs Actual Volatility", 
     ylab = "Volatility", xlab = "Date")

# Plot actual volatility (from squared returns)
lines(test_dates, sqrt(test_data^2), col = "green", lwd = 2)
legend("topright", legend = c("EWMA Forecasted Volatility", "Actual Volatility"), 
       col = c("blue", "green"), lwd = 2)





install.packages("writexl")
library(writexl)
write_xlsx(data, path = "E:/sem 5/econo/proj/Remittances-and-financial-markets/fin_data/full_dataset/dataset_vol.xlsx")

