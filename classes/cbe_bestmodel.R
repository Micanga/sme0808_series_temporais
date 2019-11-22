# 0. Importing packages
library(tseries)
library(FitAR)
library(forecast)

load <- function(edit = FALSE, show = FALSE){
	# 1. Loading the database
	CBE <- read.table('CBE.txt', header = T)

	# if you want, you can edit the CBE doc on execution
	# time using the following command
	if(edit){
		edit(CBE)
	}

	# 2. Splitting the dataset
	Elec.ts <- ts(CBE[,3], start = 1958, freq = 12)
	Beer.ts <- ts(CBE[,2], start = 1958, freq = 12)
	Choc.ts <- ts(CBE[,1], start = 1958, freq = 12)

	# 3. Plotting the extracted datasets
	if(show){	
		plot(cbind(Elec.ts, Beer.ts,Choc.ts))
		locator(1)
	}

	# 4. Returning the result
	datasets = list()
	datasets$Elec.ts = Elec.ts
	datasets$Beer.ts = Beer.ts
	datasets$Choc.ts = Choc.ts
	return(datasets)
}

mydecompose <- function(ts, show = FALSE){
	# 1. Decomposing the time series trend, seasonality
	# and random component
	ts.decom <- decompose(ts, type="mult")
	trend <- ts.decom$trend 
	seasonal <- ts.decom$seasonal
	random <- ts.decom$random

	# 2. collecting the random component and plotting
	# the differentiation result
	Z <- ts(random[7:390])

	# 3. Plotting
	if(show){
		layout(1:3)
		plot(ts(ts)) # Time Series
		ts.without.Trend = diff(ts(ts))
		plot(ts.without.Trend) # TS without trend
		plot(Z) # random component	
	}

	return(ts.decom)
}

myBIC <- function(ts.Z){
	AR1 <- arima(ts.Z, order = c(1, 0, 0))	# AR(1)
	AR2 <- arima(ts.Z, order = c(2, 0, 0))	# AR(2)
	ARMA11 <- arima(ts.Z, order = c(1, 0, 1)) # ARMA(1,1)

	BIC = BIC(AR1, AR2, ARMA11)
	BIC = BIC$BIC

	idx = which.min(BIC)
	if(idx == 1){
		return(AR1)
	}
	else if(idx == 2){
		return(AR2)
	}
	else{
		return(ARMA11)
	}
}

# 1. Fitting the model for Eletricity Dataset
ds = load()
decom.Elec = mydecompose(ds$Elec.ts)
fitted.ts = myBIC(decom.Elec$random)
print("Our Model")
print(fitted.ts)
print("##############")

# a. Residual analysis
#layout(1:2)
#acf(fit$residuals, lag.max = 30)
#pacf(fit$residuals, lag.max = 30)

# 2. Predicting the time series with the fitted one

# For time-series prediction, suggestions:
# predict.ar, predict.Arima, predict.arima0,
# predict.HoltWinters, predict.StructTS... 

# a. using predict
prev = predict(fitted.ts,6)

# b. using forecast
fZElec=forecast(fitted.ts, h=6)

# c. plotting
par(mfrow=c(2,1))

window = window(decom.Elec$random, start=370)
ts.plot(window,
	main='Previsão ZElec - Modelo AR(1)',
	prev$pred,
	prev$pred+1.96*prev$se,
	prev$pred-1.96*prev$se,
	col=c(1,2,2,2), lty=c(1,1,2,2))

ts.plot(fZElec)
locator(1)

# 3. Comparing to autoarima
par(mfrow=c(1,1))
model = auto.arima(ts(decom.Elec$random))
print("Auto Arima")
print(model)
print("###########")

fZElec=forecast(model,h=20)
plot(fZElec)