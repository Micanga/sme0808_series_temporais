library(tseries)
par(mfrow=c(2,1))

# 1. Loading the AirPassengers dataset
data(AirPassengers)
data <- AirPassengers

# 2. Presenting the dataset
print(data)
plot(data, ylab="Passagers (100's)")

# 3. Plotting the series acf
acf(data)
locator(1)

# 4. Decomposition of additive time series
# X = T + S + R
# Series = Trend + Sasonal + Random
# a. getting the time series
print(c(start(data),end(data)))
AP = list()
AP$ts <- ts(data, start = 1949, freq = 12)

# b. decomposing
decompose(AP$ts)
plot(decompose(AP$ts))
locator(1)

# 5. Decomposition of multiplicative time series
# X = T * S * R
# Series = Trend * Sasonal * Random
decompose(AP$ts, type="mult")
plot(decompose(AP$ts, type="mult"))
locator(1)

# 6. Saving the decomposition result
AP$decom <- decompose(AP$ts)
AP$trend <- AP$decom$trend 
AP$seasonal <- AP$decom$seasonal
AP$random <- AP$decom$random

# 7. Analysing the series autocorrelation
acf(AP$ts)
pacf(AP$ts)
locator(1)

Z = AP$random[13:120]
acf(Z, lag.max = 30)
pacf(Z, lag.max = 30)
locator(1)

# 8. Fitting an ARIMA model to the series
# ARIMA(p,d,q) 
fit1 <- arima(Z, order = c(1, 0, 1))
fit2 <- arima(Z, order = c(1, 0, 0))
fit3 <- arima(Z, order = c(2, 0, 0))

fits = c(fit1,fit2,fit3)

AP$evaluation = BIC(fit1, fit2, fit3)
print(AP)
