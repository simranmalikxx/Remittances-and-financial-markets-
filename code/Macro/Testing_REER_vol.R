
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

#1 .
#plotting REER

# Convert 'year' and 'month' to a proper Date format (e.g., "2000-01-01")
month_to_num <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                  "Jul" = 7, "Aug" = 8, "Sept" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
data$month_num <- month_to_num[data$month]
data$Date <- as.Date(paste(data$year, data$month_num, "01", sep="-"))
head(data$Date)

# Plot REER with Date on the x-axis
plot(data$Date, data$REER, type = "l", 
     main = "REER", ylab = "REER", xlab = "Date", 
     col = "blue", lwd = 2)



#2 . Stationarity Test
adf_test <- adf.test(na.omit(data$REER), alternative = "stationary")
print(adf_test)

#The REER plot shows significant volatility with sharp spikes and dips throughout the period, particularly in recent years (2020-2021). These fluctuations likely reflect economic shocks or policy changes. The data suggests periods of instability in the exchange rate, with occasional bursts of high volatility, possibly due to global events like the COVID-19 pandemic.
#The Augmented Dickey-Fuller (ADF) test results indicate that the REER data is stationary. With a Dickey-Fuller statistic of -7.3223 and a p-value of 0.01, we reject the null hypothesis of a unit root, meaning there is no significant trend or random walk in the data. This suggests that the REER series does not exhibit long-term trends or stochastic behavior, and its statistical properties are stable over time.


# 3. ACF
acf(na.omit(data$REER), main = "ACF of REER")
#The ACF (Autocorrelation Function) plot for the REER shows no significant autocorrelation at any lag, as all the bars are well within the blue confidence bands. This suggests that there is no serial dependence in the REER data — meaning past values of the series do not predict future values. The data appears to be independent over time, which aligns with the result of the ADF test indicating stationarity, but also implies there is no long-term correlation or volatility clustering in the series.



# 4.Test for autocorrelation
Box.test(data$REER, lag = 12, type = "Ljung")
#The Box-Ljung test results for the REER data show a p-value of 0.02779, which is less than the typical significance threshold of 0.05. This indicates that there is significant autocorrelation in the residuals of the REER data at lags up to 12.
#Despite the ACF plot showing no significant correlation, the Box-Ljung test suggests some level of dependence at higher lags, which could imply that there might still be short-term memory or patterns in the data not immediately visible in the ACF plot. This suggests that further analysis or a different model (e.g., ARMA or GARCH) might be useful to capture this autocorrelation.


# 5. Test for ARCH effects (conditional heteroskedasticity)
library(FinTS)
ArchTest(data$REER, lags = 12)

#ARCH effects indicate that the volatility of the REER data is time-varying, and past errors influence current volatility. This suggests that a model like GARCH (Generalized Autoregressive Conditional Heteroskedasticity) would be appropriate to model the volatility of REER.
#The result implies that while the mean of the REER is stationary (based on the ADF test), volatility clustering (where periods of high volatility are followed by high volatility) is present. This makes it worthwhile to use a volatility model (such as GARCH) to model the conditional variance (volatility) of the REER time series.



#6 . # Fit ARMA model to the REER data
arma_model <- auto.arima(data$REER)

# Print ARMA model summary
summary(arma_model)

# Plot the residuals of the ARMA model
plot(residuals(arma_model), main = "Residuals from ARMA model", col = "blue")

arma_residuals <- na.omit(residuals(arma_model))
acf(arma_residuals, main = "ACF of ARMA Residuals")
Box.test(arma_residuals, lag = 12, type = "Ljung-Box")

#ACF of ARMA Residuals:
#The ACF plot for the ARMA residuals shows no significant autocorrelation at any lags. All the bars are within the blue confidence bands, meaning that the residuals are random and do not exhibit any clear pattern or structure.
#This suggests that the ARMA model has adequately captured the time-dependent structure in the REER data. The residuals resemble white noise, which is ideal.
#Box-Ljung Test:
#The Box-Ljung test was performed on the ARMA residuals with 12 lags. The result shows: X-squared = 20.912 p-value = 0.05168
#Since the p-value is just slightly above 0.05, we fail to reject the null hypothesis of no autocorrelation at lags 1–12. This implies that there is no significant autocorrelation in the residuals at these lags, and the ARMA model seems to have captured the time structure adequately.

# Extract ARMA coefficients
arma_coefficients <- coef(arma_model)
data$ARMA_MA1 <- arma_coefficients["ma1"]

# Add ARMA fitted values (predicted mean)
arma_fitted <- fitted(arma_model)
data$ARMA_Fitted <- c(rep(NA, length(data$REER) - length(arma_fitted)), arma_fitted)



#7. GARCH
# Specify the GARCH(1,1) model
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # sGARCH is the standard GARCH model
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),  # No mean model, using ARMA residuals
  distribution.model = "norm"  # Assuming normal distribution for the errors
)

# Fit the GARCH(1,1) model to the residuals of the ARMA model
garch_fit <- ugarchfit(spec = garch_spec, data = arma_residuals)

# View the GARCH model summary
summary(garch_fit)

# Extract forecasted volatility (sigma values)
forecasted_volatility <- sigma(garch_fit)

# Plot the forecasted volatility (conditional volatility)
plot(forecasted_volatility, type = "l", col = "red", 
     main = "Forecasted Volatility (GARCH(1,1)) of REER", 
     ylab = "Volatility", xlab = "Time")

data$GARCH_Volatility <- c(rep(NA, length(data$REER) - length(forecasted_volatility)), forecasted_volatility)


# Check the residuals of the GARCH model
acf(residuals(garch_fit), main = "ACF of GARCH Residuals")
Box.test(residuals(garch_fit), lag = 12, type = "Ljung-Box")

install.packages("writexl")
library(writexl)
write_xlsx(data, path = "E:/sem 5/econo/proj/Remittances-and-financial-markets/fin_data/full_dataset/dataset_REER_vol.xlsx")


