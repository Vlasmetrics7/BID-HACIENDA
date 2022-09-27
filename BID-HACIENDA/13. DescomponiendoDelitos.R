# Descomposición de una serie temporal

# EJEMPLO DE DESCOMPOSICIÓN DE SERIES TEMPORALES DE FORMA AUTOMÁTICA
# ASUMIMOS QUE SOLO TIENE COMPONENTE TENDENCIAL Y ESTACIONAL

# VARIAS LIBRERIAS
# Algunos paquetes para cargar nuestros ejemplos sencillos
#install.packages("fpp3")
#install.packages("forecast")
library(fpp3)
library(forecast)
library(readxl)
library(tidyverse)

homicidios <- read_excel("Homicidios.xlsx")
homicidios<- ts(homicidios$tasa_homic,frequency=12,start=c(1997,1))

###############################
# descomposición deterministica
###############################

# USANDO DECOMPOSE( ) 

decompose_homicidios = decompose(homicidios, "additive")
plot(decompose_homicidios)

# GRÁFICAS INDIVIDUALES
# plot(as.ts(decompose_homicidios$trend))
# plot(as.ts(decompose_homicidios$seasonal))
# plot(as.ts(decompose_homicidios$random))

# USANDO STL( )

plot(stl(homicidios, "periodic"))


###############################
# descomposición con métodos estocásticos
# X13 ARIMA SEATS
###############################

#install.packages("x13binary")
#install.packages("seasonal")
#install.packages("seasonalview")
library(x13binary)
library(seasonal)
library("seasonalview")

#By default, seas calls the SEATS adjustment procedure
# VISIÓN AUTOMÁTICA DE X13-ARIMAS-SEATS
####################################################################

T0=length(homicidios)-12    # DEJAMOS LAS ÚLTIMAS 12 OBSERVACIONES PARA 
                            # EJEMPLIFICAR UN PRONÓSTICO 

#Por default "seas" corre el "SEATS adjustment procedure". Es posible cambiarlo.

homicidiosTS<- ts(homicidios[1:T0],frequency=12,start=c(1997,1))
hom.seats <- seas(homicidiosTS)

# Ambiente gráfico del X13-ARIMA-SEATS
view(hom.seats)

# Podemos ver la desestacionalización más visual y separada por componentes

plot(hom.seats$data,main="AJUSTE VIA SEATS")
summary(hom.seats)

# Una forma de especificar una función general
hom.pred=seas(
  x = homicidiosTS,
  transform.function = "log",
  regression.aictest = "td"
)

### A PARTIR DE AQUÍ LO QUE BUSCAMOS ES GENERAR UN PRONÓSTICO A 12 PERIODOS ###

hom.deses=final(hom.pred)

homicidio2<-c(homicidios[1:T0],rep(NA, 12))
homicidio2<-ts(homicidio2,frequency=12,start=c(1997,1))

homicidio3<-c(rep(NA, T0),homicidios[253:264])
homicidio3<-ts(homicidio3,frequency=12,start=c(1997,1))

Pronostico<-series(hom.pred, "forecast.forecasts")

fore12<-c(rep(NA, T0),Pronostico[1:12,1])
fore12<-ts(fore12,frequency=12,start=c(1997,1))

UppC<-c(rep(NA, T0),Pronostico[1:12,3])
UppC<-ts(UppC,frequency=12,start=c(1997,1))

LowC<-c(rep(NA, T0),Pronostico[1:12,2])
LowC<-ts(LowC,frequency=12,start=c(1997,1))

ts.plot(homicidio2,ylim=c(0.5,2.9),main="Pronosticando la tasa de homicidio doloso",xlab="",ylab="")
par(new=TRUE)
ts.plot(homicidio3,ylim=c(0.5,2.9),xlab="",ylab="",type="l",col="black",lwd=3)
par(new=TRUE)
ts.plot(fore12,col="red",ylim=c(0.5,2.9),xlab="",ylab="",lwd=3)
par(new=TRUE)
ts.plot(UppC,col="red",ylim=c(0.5,2.9),xlab="",ylab="")
par(new=TRUE)
ts.plot(LowC,col="red",ylim=c(0.5,2.9),xlab="",ylab="")
xx = c(time(UppC), rev(time(UppC))); yy = c(LowC, rev(UppC))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
