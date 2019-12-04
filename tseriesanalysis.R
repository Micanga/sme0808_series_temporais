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
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

################
# Projeto da Disciplina SME0808
# Analise de uma serie temporal
#  - Este projeto propoe a implementacao e
# teste de um programa para analise de uma
# serie temporal a fim de se ajustar o melhor
# modelo para esta.
#  - Para estudo, a serie Souvenir Sales foi
# escolhida e está disponível em:
#
#	http://robjhyndman.com/tsdldata/data/fancy.dat
#
# - Esta serie contem informacoes sobre o as
# vendas mensais de souvenir em um resort na
# praia em Queensland, Australia.
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
pdf('~/Desktop/ts.pdf', width=8, height=5)
souvenir.ts <- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenir.ts, main='Souvenir Time Series',xlab='Time',ylab='Sales')
dev.off()

# - observe que as componentes de tendencia
# nao eh muito clara no grafico apresentado.
# - para tornar esta componente mais clara,
# iremos utilizar a transformacao log da serie
# e realizar o estudo sobre esta serie.
pdf('~/Desktop/log_ts.pdf', width=8, height=5)
log.souvenir.ts <- log(souvenir.ts)
plot(log.souvenir.ts, main='Souvenir Log Time Series',xlab='Time',ylab='Sales')
dev.off()

# 2. Realizando uma decomposicao da serie e
# eliminando tendencia e sazonalidade para
# identificacao a posteriori do modelo
pdf('~/Desktop/decom.pdf', width=8, height=8)
souvenir.decom <- decompose(log.souvenir.ts)
plot(souvenir.decom)
dev.off()

X <- ts(log.souvenir.ts)
T <- ts(souvenir.decom$trend)
S <- ts(souvenir.decom$seasonal)
Z <- ts(souvenir.decom$random[7:78])

# a. eliminando a tendencia
X.stationary <- X - T
pdf('~/Desktop/stat_ts.pdf', width=8, height=5)
plot(X.stationary, main='Stationary Souvenir Time Series',xlab='Time',ylab='Sales')
dev.off()

# b. eliminando sazonalidade
X.nonsazonal <- X - S
pdf('~/Desktop/nonsea_ts.pdf', width=8, height=5)
plot(X.nonsazonal, main='Non-Seasonal Souvenir Time Series',xlab='Time',ylab='Sales')
dev.off()

# c. eliminando a tendencia
X.stationary.nonsazonal <- X - (T + S)
pdf('~/Desktop/clean_ts.pdf', width=8, height=5)
plot(X.stationary.nonsazonal, main='Souvenir Time Series without Trend and Seasonal component',xlab='Time',ylab='Sales')
dev.off()

# 3. Considerando a componente estocastica
pdf('~/Desktop/random_ts.pdf', width=8, height=5)
plot(Z,main='Souvenir Time Series Random component',xlab='Time',ylab='Sales')
dev.off()

pdf('~/Desktop/corr.pdf', width=8, height=8)
par(mfrow=c(2,1))
acf(Z, lag.max = 20)
pacf(Z, lag.max = 20)
dev.off()

# a. analisando os graficos da acf e da pacf
# os modelos a serem propostos serao
fit1 <- arima(Z, order = c(1, 0, 1))
fit2 <- arima(Z, order = c(2, 0, 1))
fit3 <- arima(Z, order = c(3, 0, 1))
fit4 <- arima(Z, order = c(1, 0, 3))
fit5 <- arima(Z, order = c(2, 0, 3))
fit6 <- arima(Z, order = c(3, 0, 3))

# 4. Selecionando o melhor modelo
print(BIC(fit1,fit2,fit3,fit4,fit5,fit6))

# a. o melhor modelo analisando o BIC e o
# ARIMA(p,d,q) com p=3,d=0,q=1
bestmodel = fit3

# 5. Certificando que o modelo possui para_
# metros significativos
pdf('~/Desktop/rescorr.pdf', width=8, height=8)
par(mfrow=c(2,1))
acf(bestmodel$residuals, lag.max = 30)
pacf(bestmodel$residuals, lag.max = 30)
dev.off()

# 6. Realizando o diagnostico do modelo
par(mfrow=c(1,1))
pdf('~/Desktop/forecast.pdf', width=8, height=6)
prev <- forecast(bestmodel,h=20)
plot(prev)
dev.off()

################
# Comparando ao autoarima
################
autoarima = auto.arima(Z,trace=TRUE)