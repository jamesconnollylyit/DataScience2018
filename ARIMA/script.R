# frequency 4 => Quarterly Data
ts(inputData, frequency = 4, start = c(1959, 2))
# freq 12 => Monthly data. 
ts(1:10, frequency = 12, start = 1990)
# Yearly Data
ts(inputData, start = c(2009), end = c(2014), frequency = 1)

# ts data
# Daily Closing Prices Of Major European Stock Indices, 1991--1998
ts_data <- EuStockMarkets[, 1]
opar <- par()
par(mfrow = c(1, 2))
# use type = "additive" for additive components
decomposed_result <- decompose(ts_data, type = "mult")
plot(decomposed_result)
decomposed_result <- decompose(ts_data, type = "additive")
plot(decomposed_result)
seasonal_trend_error <- stl(ts_data, s.window = "periodic")
par <- opar
# Examine first few rows of time series
stlRes$time.series

# shifted 3 periods earlier. Use `-3` to shift by 3 periods forward.
lagged_ts <- lag(ts_data, 3)

install.packages("DataCombine")
library(DataCombine)
# Create a data frame with 1 lag and 1 lead value
my_dataframe <- as.data.frame(ts_data)
# create lag1 variable
my_dataframe <- slide(my_dataframe, "x", NewVar = "xLag1", slideBy = -1)
# create lead1 variable 
my_dataframe <- slide(my_dataframe, "x", NewVar = "xLead1", slideBy = 1)
head(myDf)

# both acf() and pacf() generates plots by default
acf_res <- acf(AirPassengers) # autocorrelation
acf_res
# partial autocorrelation
pacf_res <- pacf(AirPassengers)

# de-trending a time series
# This example uses the Johnson & Johnson dataset
# Recall that lm() used to build linear model
plot(JohnsonJohnson)
trained_model <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))
# resid(trained_model) contains the de-trended series.
plot(resid(trained_model), type = "l")

# De-seasonalise a time series
library(forecast)
# decompose the time series
# Either the character string “periodic” or the span (in lags) 
# of the loess window for seasonal extraction is entered in function
? stl
ts_decompose <- stl(AirPassengers, "periodic")
# de-seasonalise the time series
ts_seasonal_adjust <- seasadj(ts_decompose)
# original series
plot(AirPassengers, type = "l")
# seasonal adjusted
plot(ts_seasonal_adjust, type = "l")
# seasonal frequency set as 12 for monthly data.
seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal plot: Airpassengers")

# Test if a time series is stationary
library(tseries)
# p-value < 0.05 indicates the TS is stationary
adf.test(ts_data)
? kpss.test
# Computes the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
# for the null hypothesis that x is level or trend stationary
kpss.test(ts_data)

# Seasonal Differencing
# number for seasonal differencing needed
nsdiffs(AirPassengers)
# Apply 1 seasonal differencing
AirPassengers_seasdiff <- diff(AirPassengers, lag = frequency(AirPassengers), differences = 1)
plot(AirPassengers_seasdiff, type = "l", main = "Seasonally Differenced")

# Make it stationary
# number of differences need to make it stationary
ndiffs(AirPassengers_seasdiff)
stationaryTS <- diff(AirPassengers_seasdiff, differences = 1)
plot(stationaryTS, type = "l", main = "Differenced and Stationary")

# ARIMA and Nile forecasting example
# First you plot the time series and assess its stationarity
library(forecast)
library(tseries)
plot(Nile)
# Assess presence of a trend in dataset
ndiffs(Nile)
# Since there is a trend, the series is differenced 
# once (lag=1 is the default) and saved as dNile
d_nile <- diff(Nile)
# Plot the differenced time series
plot(d_nile)
ndiffs(d_nile)
# Applying the ADF test to the differenced series 
# suggest that it’s now stationary, so we can 
# proceed to the next step
adf.test(d_nile)

# Identifying one or more reasonable models
# here we examine autocorrelation and partial
# autocorrelation plots for the differenced
# Nile time series
# autocorrelatioin plot
Acf(d_nile)
# partial autocorrelation plot
Pacf(d_nile)

# Fitting an ARIMA model -------------------------------------
# Note we use the original dataset for the ARIMAA model
# and modify the d value to suit our earlier findings
library(forecast)
fit <- Arima(Nile, order = c(0, 1, 1))
fit
# Accuracy measures
accuracy(fit)

# Evaluating model fit ---------------------------------------
# qqnorm produces a normal QQ plot of the values in y. 
# qqline adds a line to a “theoretical”, quantile-quantile plot 
# which passes through the probs quantiles, 
# by default the first and third quartiles
help("qqnorm")
qqnorm(fit$residuals)
qqline(fit$residuals)
# Box.test() function provides a test that autocorrelations 
# are all zero. The results aren ’t significant, suggesting 
# the autocorrelations don ’t differ from zero.
# This ARIMA model appears to fit the data well.
Box.test(fit$residuals, type = "Ljung-Box")

# Forecast 3 years ahead for Nile time series
forecast(fit, 3)
# Plot function shows the forecast. Point estimates are
# given by the blue dots, 80 % and 95 % confidence bands 
# are represented by dark and light bands, respectively
plot(forecast(fit, 3), xlab = "Year", ylab = "Annual Flow")

# Automated ARIMA forecasting -------------------------------
# Comparing the automatic test against our manual method above
library(forecast)
fit <- auto.arima(Nile)
fit

accuracy(fit)
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")

plot(forecast(fit, 3), xlab = "Year", ylab = "Annual Flow")
