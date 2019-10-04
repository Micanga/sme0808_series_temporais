require(forecast)

# Exercise 1 - List 1
ns.gaussian <- function(){
	# 1. Generating the series a, b and c
	time = seq(1,200,1)
	st.a = c()
	st.b = c()
	st.c = c()
	for(t in time){
		st.a = c(st.a,10*exp(-(t-100)/20)*cos((2*pi*t)/4))
		st.b = c(st.b,10*exp(-(t-100)/200)*cos((2*pi*t)/4))
		st.c = c(st.c,10*cos((200*pi*t)/4))
	}
	par(mfrow=c(3,3))
	plot(time,st.a,type='l')
	plot(time,st.b,type='l')
	plot(time,st.c,type='l') 

	# 2. Creating a Gaussian noise
	gaussian.noise = rnorm(200, 0,1)

	# 3. Generating the signal st + wt
	signal.a = st.a + gaussian.noise
	signal.b = st.b + gaussian.noise
	signal.c = st.c + gaussian.noise
	plot(st.a + gaussian.noise,type='l')
	plot(st.b + gaussian.noise,type='l')
	plot(st.c + gaussian.noise,type='l') 

	# 4. Applaying a moving average filter
	filter.signal.a = ma(signal.a, 4)
	filter.signal.b = ma(signal.b, 4)
	filter.signal.c = ma(signal.c, 4)
	plot(signal.a,type='l')
		lines(filter.signal.a,type='l',lty=2, col=2)
	plot(signal.b,type='l')
		lines(filter.signal.b,type='l',lty=2, col=2)
	plot(signal.c,type='l') 
		lines(filter.signal.c,type='l',lty=2, col=2)

}

# Exercise 2 - List 1
ns.gaussbased <- function(){
	# 1. Generating the time series
	time = seq(1,200,1)
	st = c()
	for(t in time){
		st = c(st, cos((2*pi*t)/4))
	}
	par(mfrow=c(3,1))
	plot(time,st,type='l')

	# 2. Creating a noisy zt = -0.9z_{t-1} + Gaussian noise
	zt = c(0)
	for(t in 2:length(time)){
		zt = -0.9*zt[t-1] + rnorm(200, 0,1)
	}

	# 3. Generating the signal st + wt
	signal = st + zt
	plot(st + zt,type='l')

	# 4. Applaying a moving average filter
	filter.signal = ma(signal, 4)
	plot(signal,type='l')
		lines(filter.signal,type='l',lty=2, col=2)

}

# Exercise 3 - List 1