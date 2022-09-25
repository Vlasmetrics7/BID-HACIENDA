# MODELO LINEAL DE TAYLOR

# Leemos la base de datos de R
load("taylor.RData")


# Elegimos la serie con la que trabajeremos 1987:Q1 to 1999:Q4

infl <- ts(taylor[104:nrow(taylor), 2],frequency = 12, start=c(1987,3))  
ygap <- ts(taylor[104:nrow(taylor), 3],frequency = 12, start=c(1987,3))  
ffr  <- ts(taylor[104:nrow(taylor), 4],frequency = 12, start=c(1987,3))  

#######################
# Gráfica de las series
#######################

xs <- as.Date(taylor[,1], origin="0000-01-01")
plot (xs, taylor[,4], type="l", col="green", xaxs="i", yaxs="i",
      main = "US Rates",
      xlab = "",
      ylab = "",
      ylim = c(-10,20),
      bty = "l", lty = 1)
lines(xs, taylor[,2], col="blue", lty = 3)
lines(xs, taylor[,3], col="red", lty = 5)
legend("topright", 
       c("Federal funds rate","Inflation rate", "Output gap rate"), 
       lty = c(1,3,5),
       col = c("green", "blue", "red"), lwd = 1)

########################
# ESTIMACIÓN DEL MODELO
########################

reg <- lm(ffr~infl+ygap)
summary(reg)

### PREDICCIÓN ####

prediction <- ts(predict(reg),frequency = 12, start=c(1987,3))  
residuales<-  ts(residuals(reg),frequency = 12, start=c(1987,3))  

par(mfrow=c(2,1))
plot.ts(ffr, type="l", col="blue",
        main = "Federal fund Rate",
        xlab = "",
        ylab = "",
        ylim = c(2,11),
        bty = "l", lty = 1)
par(new=T)
plot.ts(prediction, col="red",xlab = "",ylab = "",
        ylim = c(2,11), lty = 3)
legend("topright", 
       c("Federal funds rate","Predicción"), 
       lty = c(1,3),
       col = c("blue", "red"), lwd = 1)
plot.ts(residuales,main="Residuales",xlab="",ylab="",lwd=2,col="black",lty=3)
abline(h=0)


#####################
# PRUEBAS DE HIPÓTESIS
#####################

#######################
# Pruebas  individuales
#######################

#if (!require('devtools')) install.packages('devtools')
#devtools::install_github('fhernanb/model', force=TRUE)
library(model)
beta_test(object=reg, parm='infl', ref.value=1.5, alternative='two.sided')
beta_test(object=reg, parm='ygap', ref.value=0.5, alternative='two.sided')


#####################
# PRUEBAS CONJUNTAS 
#####################

library(car)
linearHypothesis(reg, c("infl=1.5","ygap=0.5"))


#####################
# ALGUNOS GRÁFICOS DE PRUEBAS DE DIAGNÓSTICO
######################

# Unas gráficas automáticas de R
plot(reg, which = c(1:3,5))

# Podemos generar las nuestras
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot.ts(reg$resid,
     main="Modelo lineal de Taylor",
     xlab="", ylab="Residuals")
abline(h=0,lty=2)
#Histograma de Residuales
hist(reg$resid, main="Histograma de Residuales",xlab="",
     ylab="Residuales")
#Q-Q Plot
qqnorm(reg$resid)
qqline(reg$resid)

