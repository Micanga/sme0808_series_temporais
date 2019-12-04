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
AirQuality.CO <- AirQuality.csv[6:2120,3]

################
# Projeto da Disciplina SME0808
# Analise de uma serie temporal
#  - Este projeto propoe a implementacao e
# teste de um programa para analise de uma
# serie temporal a fim de se ajustar o melhor
# modelo para esta.
#  - Para estudo, a serie AirQualityUCI foi
# escolhida. Disponivel em:
#
# 	https://archive.ics.uci.edu/ml/
#		machine-learning-databases/00360/
#
# - Esta serie e uma serie multivariada.
# Desta forma, para fins de avaliacao, iremos
# estuda-la de maneira univariada, anali_
# sando a variavel CO(GT) que apresenta a
# cencentracao media real por hora de CO
# em mg/m^3.
# - O roteiro de diagnostico e identificacao 
# de um modelo ARIMA(p,d,q) para uma serie
# temporal usado neste projeto sera o roteiro
# apresentado em aula.
################
# Diagnostico e identificacao da ST
################
# 1. Criando o grafico da serie para iden_
# tificacao de tendencia, sazonalidade e
# heterocedasticidade, assumindo:
#
#	X_t = T_t + S_t + Z_t
#
AirQuality.ts <- ts(AirQuality.CO,freq=12)
plot(AirQuality.ts)
locator(1)

# 2. Eliminando tendencia e sazonalidade
# da serie
AirQuality.decom <- decompose(AirQuality.ts)
plot(AirQuality.decom)
locator(1)

X <- AirQuality.ts
T <- ts(AirQuality.decom$trend)
S <- ts(AirQuality.decom$seasonal)
Z <- ts(AirQuality.decom$random[7:2109])

# 3. Considerando a componente estocastica
par(mfrow=c(1,1))
plot(Z)
locator(1)

par(mfrow=c(2,1))
acf(Z, lag.max = 20)
pacf(Z, lag.max = 20)
locator(1)

# a. analisando os graficos da acf e da pacf
# os modelos a serem propostos serao
fit1 <- arima(Z, order = c(13, 0, 8))
fit2 <- arima(Z, order = c(15, 0, 8))
fit3 <- arima(Z, order = c(13, 0, 8))

# 4. Selecionando o melhor modelo
print(BIC(fit1,fit2,fit3))

# a. o melhor modelo analisando o BIC e o
# ARIMA(p,d,q) com p=13,d=0,q=8
bestmodel = fit1

# 5. Certificando que o modelo possui para_
# metros significativos
par(mfrow=c(2,1))
acf(bestmodel$residuals, lag.max = 30)
pacf(bestmodel$residuals, lag.max = 30)
locator(1)

# 6. Realizando o diagnostico do modelo
prev <- forecast(bestmodel,h=100)
par(mfrow=c(2,1))
plot(prev)

################
# Comparando ao autoarima
################
autoarima = auto.arima(Z, trace=TRUE)

# O auto arima indica que o melhor modelo
# e o AR(1), contudo, seguindo nosso diag_
# nostico, provamos que este nao representa
# o melhor modelo para esta serie.
prev <- forecast(autoarima,h=100)
plot(prev)