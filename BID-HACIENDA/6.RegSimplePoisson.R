
# EJEMPLO DE MODELO POISSON

#install.packages("Ecdat")   #paquete que incluye muchas bases de datos del Journal of Econometrics
library(Ecdat)
data(StrikeNb)
# Kennan, J. (1985). The duration of contract strikes in US manufacturing. Journal of Econometrics, 28(1), 5-28.
View(StrikeNb)

hist(StrikeNb$strikes, breaks = 15,
     probability = T ,main = "Histograma típico de una variable de conteo",xlab="Huelgas")
lines(density(StrikeNb$strikes), col="red", lwd=2)

##### GR?FICA DE DOS EJES Y ###############
library(latticeExtra)

data <- data.frame(StrikeNb$time,StrikeNb$strikes,StrikeNb$output)
colnames(data)<-c("Time","Strikes","Output")

obj1 <- xyplot(Strikes ~ Time, data, type = "l" , lwd=2,xlab="Time",ylab="Strikes")
obj2 <- xyplot(Output ~ Time, data, type = "l", lwd=2,xlab="Time",ylab="Output")
doubleYScale(obj1, obj2, text = c("Strikes", "Output"),add.ylab2 = TRUE)
##########################################

# ¿por qué no es tan buena idea asumir u~N(0,sigma)?

##########################################

# En este ejemplo buscamos analizar el efecto del nivel de la actividad económica (output) 
# sobre la frecuencia mensual de huelgas de las empresas norteamericanas (strike) 
# desde enero 1958 a diciembre 1976
# Outputs elevados indican "boom" de la economía, mientras que valores bajos indican "recesiones"


# el modelo sería mu=exp(theta0+output*theta1), donde mu mide la (frecuencia) de huelgas mensuales
# asumimos que sigue la media de una distribución poisson con theta0  y theta1 
# parámetros desconocidos

# Usamos el comando glm 
# glm: Fitting Generalized Linear Models

StrikePoisson <-glm(formula = Strikes ~ Output,
             data, family = poisson)

pred.poisson <- StrikePoisson$fitted.values  #predicci?n


# Estimamos una regresi?n lineal, estimamos v?a OLS
lm.fit <-  lm(StrikeNb$strikes~StrikeNb$output)
pred.lm <- predict(lm.fit)


#Graficamos ambas
#install.packages(dygraphs)

library(dygraphs)

DataPreds <- data.frame(
  time=StrikeNb$time, 
  Strikes=StrikeNb$strikes, 
  Poisson=pred.poisson,
  Linear=pred.lm
)

p <- dygraph(DataPreds,main = "Huelgas reales vs predicciones")
p






