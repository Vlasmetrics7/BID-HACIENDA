# EJEMPLO REGRESION LINEAL

library(ggplot2)
library(readxl)
NFCI <- read_excel("NFCI.xlsx")
data<-data.frame(NFCI)

GrowthMX<- ts(NFCI$MEXICO, start = c(2005,3), frequency = 4)
NFCI<- ts(NFCI$NFCI, start = c(2005,3), frequency = 4)
par(mfrow=c(2,1))
plot.ts(GrowthMX,main="Crecimiento económico México",xlab="")
plot.ts(NFCI,main="National Financial Condition Index",xlab="")

############## Visualizando residuales ###########

### aquí puede seguir algunos ejemplos
###  https://rpubs.com/mglantz/690906

fit <- lm(MEXICO ~ NFCI, data)

summary(fit)
confint(fit)

data$predicted <- predict(fit)   # Guardamos los valores ajustados
data$residuals <- residuals(fit) # Guardamos los residuales


q<-ggplot(data, aes(x = date , y = MEXICO)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = date, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +  
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  
  guides(scale = "none") +
  geom_point(aes(y = predicted), shape = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  theme_bw()

q+ggpubr::rotate_x_text()


######################
#### PARTE 1 #########
# NORMALIDAD #
######################
# EN EL SIGUIENTE PAQUETE TENEMOS VARIAS PRUEBAS RELEVANTES DE SERIES DE TIEMPO
#install.packages("tseries")

library("tseries")

### HISTOGRAM
par(mfrow=c(2,1))
hist(residuals(fit),main = "Histograma de residuales",xlab="")
### QQPLOT
qqnorm(residuals(fit), pch = 1, frame = FALSE)
qqline(residuals(fit), col = "steelblue", lwd = 2)

### JARQUE BERA
### H0: NORMALIDAD 

jarque.bera.test(residuals(fit))


######################
#### PARTE 2 #########
# HETEROCEDASTICIDAD #
######################

# Probamos Heterocedasticidad

#install.packages("lmtest")
library(lmtest)
library(AER)
gqtest(fit)   #Goldfeld-Quandt test
bptest(fit)   #Breusch-Pagan test
bptest(fit, MEXICO ~ NFCI + I(NFCI^2),data = data)  #White test
  

# Corrijamos la heterocedasticidad

# cálculo de "heteroskedasticity-robust standard errors" con el tipo HC1
# vcov es la matriz de covarianza robusta de los coeficientes
# robust_se son las desv est robustas de los coeficientes

vcov <- vcovHC(fit, type = "HC1")
vcov
robust_se <- sqrt(diag(vcov))
robust_se

summary(fit)
coeftest(fit, vcov. = vcov)

library(stargazer)
stargazer(fit,fit,title="Resultados",column.labels = c("No Robusto","Robusto"),
          se = list(NULL,robust_se),align=TRUE,no.space=TRUE,type="text",
          notes.label = "Niveles de significancia",ci=TRUE)


######################
#### PARTE 3 #########
# AUTOCORRELACIÓN #
######################
RES<-residuals(fit)
#### REVISEMOS EL SUPUESTO DE AUTOCORRELACIÓN
## Mediante el Correlograma: Función de autocorrelación de residuales

acf(RES,main='Función de autocorrelación de residuales')

## Mediante 

# Durbin-Watson test
# H0: NO AUTOCORRELACIÓN

dwtest(GrowthMX ~ NFCI)

# Breusch-Godfrey test
# H0: NO AUTOCORRELACIÓN

bgtest(GrowthMX ~ NFCI, order=1)


##### AHORA ARREGLEMOS EL PROBLEMA

# Comandos útiles del paquete sandwich: 
# vcovHAC(): Heteroskedasticity- and autocorrelation-consistent (HAC) estimators 
# NeweyWest(): Estimador HAC de Newey-West

#install.packages('sandwich')
library(sandwich)
#?NeweyWest
m<-5
NW_VCOV <- NeweyWest(lm(GrowthMX ~ NFCI),lag=m-1, prewhite = F, adjust = T)
NW_VCOV   #Matriz de covarianza de Newey-West

# Error estandar MCO vs Newy-West
c(sqrt(diag(vcov))[2],sqrt(diag(NW_VCOV))[2])
