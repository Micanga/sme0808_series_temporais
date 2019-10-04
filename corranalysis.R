# Exercise 4 - List 1
ca.coinseries <- function(){
	# 1. Generating the time series
	time = seq(1,200,1)
	wt = c(0)
	for(t in 2:length(time)){
		if(runif(1) < 0.5){
			wt = c(wt,  1 - 0.8*wt[t-1])
		}
		else{
			wt = c(wt, -1 - 0.8*wt[t-1])
		}
	}
	xt = wt
	par(mfrow=c(3,1))
	plot(time,xt,type='l')

	# 2. Plotting the series acf and pacf
	acf(xt)
	pacf(xt)
}