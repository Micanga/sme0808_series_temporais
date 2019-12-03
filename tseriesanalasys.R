################
# Pacotes e dependencias
################
# 1. Instalando os pacotes necessarios
#install.packages(tseries)
#install.packages("FitAR")
#install.packages("forecast")

# 2. Importando os pacotes necessarios
library(tseries)
library(FitAR)
library(forecast)

# 3. Carregando o arquivo principal
AirQuality.csv <- read.csv(
	'./AirQualityUCI/AirQualityUCI.csv',
	header=TRUE, sep=';')
AirQuality.CO <- AirQuality.csv[6:4160,3]

################
# Projeto da Disciplina SME0808
# Analise de uma serie temporal
#  - Este projeto propoe a implementacao e
# teste de um programa que analise uma se_
# rie temporal a fim de se ajustar o melhor
# modelo para a mesma.
#  - Para estudo, a serie AirQualityUCI foi
# escolhida. Disponivel em:
#
# 	https://archive.ics.uci.edu/ml/
#		machine-learning-databases/00360/
#
# - Esta serie e uma serie multivariada.
# Desta forma, a fins de avaliacao, iremos
# estuda-la de maneira univariada, anali_
# sando a variavel CO(GT) que apresenta a
# cencentracao media real por hora de CO
# em mg/m^3.
# - Sera aplicado o roteiro de diagnostico
# e identificacao de um modelo ARIMA para 
# uma serie temporal apresentado em aula.
################
# Diagnostico e identificacao da ST
################
# 1. Criando o grafico da serie para iden_
# tificacao de tendencia, sazonalidade e
# heterocidade, assumindo:
#
#	X_t = T_t + S_t + Z_t
#
AirQuality.ts <- ts(AirQuality.CO[1:4160],freq=12)
plot(AirQuality.ts)
locator(1)

# 2. Eliminando tendencia e sazonalidade
# da serie
AirQuality.decom <- decompose(AirQuality.ts, type="mult")
plot(AirQuality.decom)
locator(1)

S <- AirQuality.ts
T <- ts(AirQuality.decom$trend)
S <- ts(AirQuality.decom$seasonal)

# 3. Considerando a componente estocasticac
par(mfrow=c(1,1))
plot(Z)
locator(1)

par(mfrow=c(2,1))
acf(Z, lag.max = 20)
pacf(Z, lag.max = 20)
locator(1)

# a. analisando os graficos da acf e da pacf
# os modelos a serem propostos serao
fit1 <- arima(Z, order = c(1, 0, 0))
fit2 <- arima(Z, order = c(2, 0, 0))
fit3 <- arima(Z, order = c(1, 0, 4))
fit4 <- arima(Z, order = c(2, 0, 4))
fit5 <- arima(Z, order = c(1, 0, 5))
fit6 <- arima(Z, order = c(2, 0, 5))

# 4. Selecionando o melhor modelo
print(BIC(fit1,fit2,fit3,fit4,fit5,fit6))

# a. o melhor modelo analisando o BIC e o
# ARIMA(p,d,q) com p=2,d=0,q=4
bestmodel = arima(Z, order = c(2, 0, 4))

# 5. Certificando que o modelo possui para_
# metros significativos
par(mfrow=c(2,1))
acf(bestmodel$residuals, lag.max = 30)
pacf(bestmodel$residuals, lag.max = 30)
locator(1)

# 6. Realizando o diagnostico do modelo
prev <- predict(bestmodel,12)

#par(mfrow=c(1,2))
par(mfrow=c(2,1))
ts.plot(window(Z, start=250),main='Previsão Z - Modelo ARIMA(2,0,4)',
prev$pred,
prev$pred+1.96*prev$se,
prev$pred-1.96*prev$se,
col=c(1,2,2,2), lty=c(1,1,2,2))

################
# Comparando ao autoarima
################
print(auto.arima(Z))

# O auto arima indica que o melhor modelo
# e o AR(1), contudo, seguindo nosso diag_
# nostico, provamos que este nao representa
# o melhor modelo para esta serie.
prev <- predict(arima(Z, order = c(1, 0, 0)),12)
ts.plot(window(Z, start=250),main='Previsão Z - Modelo ARIMA(1,0,0)',
prev$pred,
prev$pred+1.96*prev$se,
prev$pred-1.96*prev$se,
col=c(1,2,2,2), lty=c(1,1,2,2))