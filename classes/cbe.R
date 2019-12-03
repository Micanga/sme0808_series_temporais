par(mfrow=c(3,1))

# 1. Loading the data and enabling to edit
CBE <- read.table('CBE.txt', header = T)
edit(CBE)

# 2. Splitting the data
Elec.ts <- ts(CBE[,3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[,2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[,1], start = 1958, freq = 12)

# 3. Plotting the data
plot(Elec.ts)
plot(Beer.ts)
plot(Choc.ts)

# 4. Plotting the Electricity acf and pacf
# a. complete time series
layout(1:2)
acf(Elec.ts)
pacf(Elec.ts)
locator(1)

# b. spliting the series
Elec.decom <- decompose(Elec.ts, type="mult")
Elec.Trend <- Elec.decom$trend 
Elec.Seasonal <- Elec.decom$seasonal
Elec.Random <- Elec.decom$random
plot(Elec.decom)
locator(1)

ZElec <- ts(Elec.Random[7:390])

# c. random component
layout(1:2)
acf(ZElec, lag.max = 30)
pacf(ZElec, lag.max = 30)
locator(1)

# 5. Fitting an ARIMA model to this series
fit1 <- arima(ZElec, order = c(1, 0, 1))
fit2 <- arima(ZElec, order = c(1, 0, 0))
fit3 <- arima(ZElec, order = c(2, 0, 0))

BIC(fit1, fit2, fit3)

evaluation = BIC(fit1, fit2, fit3)
print(evaluation)

# 6. Evaluating the model
layout(1:2)
acf(fit1$residuals, lag.max = 30)
pacf(fit1$residuals, lag.max = 30)
