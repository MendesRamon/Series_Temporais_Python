#Séries Temporais e Análises Preditivas

#pacotes
install.packages("forecast")
install.packages("ggplot2")

library(forecast)
library(ggplot2)

#cria uma serie temporal normalmente distribuida
serie1<- rnorm(60) #gera números aleatórios seguindo a dist normal
serie1<- ts(serie1, start=c(2012,1), end=c(2016,12), frequency=12)#é utilizada para trabalhar com séries temporais
plot(serie1)

#set path (diretório)
setwd("C:/Users/Lenovo/Desktop/R/3 - MODELOS ESTATÍSTICOS/Series Temporais/")
getwd()

#dataset
dados1 <- read.csv2("DadosEmpregoIA.csv")

#série temporal com tendência e sazonalidade
serie2 = ts(dados1$Comercio,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie2)
plot(serie2)

#sem tendência e com sazonalidade
serie3 = ts(dados1$Alimento,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie3)
plot(serie3)

#talvez tendência
serie4 = ts(dados1$Metalurgia,start = c(2003,1), end=c(2007,12), frequency=12)
class(serie4)
plot(serie4)


#Gráficos no R

#Análise série 1
plot(serie1)
hist(serie1)
boxplot(serie1)

serie1

#ggplot
autoplot(serie1)

#explorando parte da série
serie2a = window(serie2,2003,c(2004,12))
plot(serie2a)
hist(serie2a)
boxplot(serie2)

#Modelo Média Móvel (MM)
#aplicado nas séries de nível (não possuem tendencias nem sazonalidades)

autoplot(serie4)

#média móvelcom order 3
seriemm1 = ma(serie4, 3)
autoplot(seriemm1)

#média móvelcom order 5
seriemm2 = ma(serie4, 5)
autoplot(seriemm2)

#média móvelcom order 12
seriemm3 = ma(serie4, 12)
autoplot(seriemm3)

#comparando as 3 modelagem - observando a suavização
plot(serie4) #serie original
lines(seriemm1, col="red")#comando de lines para incluir linhas no gráfico
lines(seriemm2, col="blue")
lines(seriemm3, col="green")
#legenda
legend("topleft", legend=c("orig.", "mm3", "mm5", "mm12"), 
       col = c("black", "red", "blue", "green"), 
       lty = 1:2, cex=0.8)
#
#
#

#Alisamento Exponencial Simples - ETS (erro, tendencia, sazonalidade) - pacote Forecast

modelo1 <- ets(serie4,"ANN") #ajustando modelo
modelo1

autoplot(modelo1$residuals)
autoplot(modelo1$fitted)

prev <- forecast(modelo1, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)

#Alisamento Exponencial Duplo

modelo2 <- ets(serie2, "AAN")
modelo2

autoplot(modelo2$residuals)
autoplot(modelo2$fitted)

prev <- forecast(modelo2, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)


#Alisamento Exponencial - Holt Winters - Aditivo

modelo4 <- ets(serie2, "AAA")
modelo4

autoplot(modelo4$residuals)
autoplot(modelo4$fitted)

prev <- forecast(modelo4, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)

#Alisamento Exponencial - Holt Winters - Multiplicativo

modelo5 <- ets(serie2, "MAM")
modelo5

autoplot(modelo5$residuals)
autoplot(modelo5$fitted)

prev <- forecast(modelo5, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)

#Alisamento Exponencial - Holt Winters - Aditivo

modelo6 <- ets(serie3, "ANA")
modelo6

autoplot(modelo6$residuals)
autoplot(modelo6$fitted)

prev <- forecast(modelo6, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)

#Alisamento Exponencial - Holt Winters - Multiplicativo

modelo7 <- ets(serie3, "MNM")
modelo7

autoplot(modelo7$residuals)
autoplot(modelo7$fitted)

prev <- forecast(modelo7, h=16, levels=c(85,90)) #previsão para 16 meses
print(prev$mean)

plot(prev$mean)
autoplot(prev)


#Comparando Modelos

treino <- window(serie2, 2003, c(2005, 12))
teste <- window(serie2, 2006, c(2007, 12))

#alisamento exponencial  - holt - Winters - Aditivo

modelo8 <- ets(treino, "AAA")
modelo8

autoplot(modelo8$residuals)
autoplot(modelo8$fitted)

prev1 = forecast(modelo8, h=24)
print(prev1$mean)
autoplot(prev1)

#alisamento exponencial  - holt - Winters - Multiplicativo

modelo9 <- ets(treino, "MAM")
modelo9

autoplot(modelo9$residuals)
autoplot(modelo9$fitted)

prev2 = forecast(modelo9, h=24)
print(prev2$mean)
autoplot(prev2)


#################################################################################

plot(serie2)
lines(prev1$mean, col="blue")
lines(prev2$mean, col="red")
lines(teste, col="green")
legend("topright",legend=c("AAA","MAM","Teste"), col = c("blue","red","green"), lty=1:2, cex=0.8)

accuracy(prev1,teste )
accuracy(prev2,teste )








