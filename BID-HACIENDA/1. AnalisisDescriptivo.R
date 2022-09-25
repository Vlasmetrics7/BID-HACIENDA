
# An�lisis Descriptivo

# Instalamos paquetes
install.packages(c("readxl","dplyr","MASS"))

# Activamos librerías a usar
library(dplyr) 
library(MASS) 
library(readxl)

# A veces debemos especifir el directorio donde est�n nuestros archivos
# Usaremos:
# setwd("C:/......")

# Leemos nuestros datos
database <- read_excel("base.xlsx",sheet = "Base")

# Hemos cargado nuestra base de datos en database
# Al usar el símbolo de pesos, ejemplo Y$X, le estamos diciendo a R que queremos 
# leer la variable X que vive en la base Y

FactorTipoEnt<-factor(database$TIPOENT)
EDAD<-database$EDAD

# PROMEDIO DE EDAD POR TIPO DE ENTREVISTADO 
tapply(EDAD, FactorTipoEnt, mean)

# Conteo de número de ciudades
table(database$CIUDAD)

##### Estadísticas descriptivas básicas

(mean(database$EDADEXA))    # promedio
(median(database$EDADEXA))  # mediana
(var(database$EDADEXA))     # varianza
(sd(database$EDADEXA))      # desviación estándar
(IQR(database$EDADEXA))     # rango intercuantil
(min(database$EDADEXA))     # mínimo
(max(database$EDADEXA))     # máximo
(range(database$EDADEXA))   # rango
(summary(database$EDADEXA)) # descriptivos resumen


#####################################################
### VARIAS GRÁFICAS ###

# Histograma por default
hist(database$EDADEXA)

# Histograma pensado como probabilidad en vez de frecuencia
# y ajuste de densidad
hist(database$EDADEXA,probability=TRUE)
lines(density(database$EDADEXA),col="red")

# Histograma un poco más bello
hist(database$EDADEXA,
     xlab = "Edad", ylab = "Frecuencia",
     main = "Histograma de edad exacta",
     breaks = 8,
     col = "dodgerblue",
     border = "darkorange")

# Distribución acumulada
f <- ecdf(database$EDADEXA)
plot(f,main="Función de distribución empirica acumulativa de la edad exacta")

# Gráfica de barras por default
barplot(table(database$CIUDAD))

# Gráfica de barras algo más bello
barplot(table(database$CIUDAD),
        xlab = "1: Valle.Mex, 2: MtyA, 3:MtyB, 4:Gdl, 5:Tij, 6:Pue, 7:Qro", 
        ylab = "Frequencia",
        main = "Ciudad",
        col = "dodgerblue",
        border = "darkorange")

# Boxplot (diagrama de caja) 

boxplot(database$EDADEXA)

# Varios boxplots juntos

boxplot(database$EDADEXA ~ database$SEXO,ylab="Edad Exacta", xlab="Sexo")

# Boxplot más bello

boxplot(database$EDADEXA ~ database$SEXO,
        xlab = "Sexo: 1 es masculino y 2 es femenino",
        ylab = "Edad exacta",
        main = "Boxplot de edad exacta por sexo",
        pch = 20,
        cex = 2,
        col = "darkorange",
        border = "dodgerblue")

# Gráfica de Pie

pie(table(database$SEXO),labels=c("Masculino","Femenino"))

#####################################

# SCATTERPLOT (diagrama de dispersión)

plot(database$EDADEXA ~ database$P14,
     xlab = "Calificación",
     ylab = "Edad exacta",
     main = "Dispersión",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

# Matriz básica de dispersión

pairs(~EDADEXA+P1+P14,data=database,main="Simple Scatterplot Matrix")


# Existen otras formas más bellas de obtener una matriz de dispersión
# Recuerden que este tipo de matrices aportan mucha información de manera resumida
# Las veremos más adelante

######################
### UN EJEMPLO MACRO
######################

#install.packages('WDI')     #PAQUETE PARA EXTRAER DATOS DEL BANCO MUNDIAL
library(WDI)

# GDP (current US$) - World Bank Data. Obtenemos para México y Brasil

datMX = WDI(indicator='NY.GDP.MKTP.CD', country=c('MEX'), start=1980, end=2021)
datBRA = WDI(indicator='NY.GDP.MKTP.CD', country=c('BRA'), start=1980, end=2021)

# Reordenamos series temporales porque vienen de cabeza
PIBMX<-rev(datMX$NY.GDP.MKTP.CD)
PIBBRA<-rev(datBRA$NY.GDP.MKTP.CD)

# Calculamos logs y definimos las variables como series temporales

PIBMX<-ts(log(PIBMX), frequency = 1, start=c(1980,1))
PIBBRA<-ts(log(PIBBRA), frequency = 1, start=c(1980,1))

# GRAFICAMOS LAS DOS SERIES JUNTAS

# Podemos hacerlo de una forma sencilla
par(mfrow=c(2,1))
plot.ts(PIBMX,col="blue",xlab="")
plot.ts(PIBBRA,col="red",xlab="")

# de esta otra
plot.ts(PIBMX,col="blue",ylab="",xlab="",ylim=c(25,30))
par(new=T)
plot.ts(PIBBRA,col="red",ylab="",xlab="",ylim=c(25,30))
legend("topleft",legend=c("PIB MX","PIB BRA"),
       text.col=c("blue","red"),lty=1:2,col=c("blue","red"),cex=0.7)


# o de manera muy llamativa usando paquetes especiales
#install.packages("dygraphs")
library(dygraphs)

data <- cbind(PIBBRA,PIBMX)
p <- dygraph(data)
p


#### ¿Y SI BUSCAMOS ALGO ASÍ COMO EL CRECIMIENTO ?

tasaMX<-100*diff(PIBMX)
tasaBRA<-100*diff(PIBBRA)

plot(tasaMX,xlim=c(1980,2021),ylim=c(-50,50),ylab="",xlab="")
par(new=TRUE)
plot(tasaBRA,col="red",xlim=c(1980,2021),ylim=c(-50,50),ylab="",xlab="")
legend("topleft",legend=c("tasa PIB MX","tasa PIB BRA"),
       text.col=c("black","red"),lty=1:2,col=c("black","red"),cex=0.7)

# install.packages("car")
# library(car)
scatterplot(tasaMX~tasaBRA,regLine=TRUE,ellipse=TRUE,smooth=FALSE)




