
# EJEMPLO DE MODELO POISSON
# DATOS PROVENIENTES DE LA ENOE (Encuesta Nacional de Ocupación y Empleo) 2T 2022

#Horas trabajadas en la semana
#Ingreso mensual

library(readxl)
database <- read_excel("ENOE.xlsx",sheet = "ENOE")

hist(database$hrsocup, breaks = 15,
     probability = T ,main = "Histograma horas laborales por semana",xlab="Número")

##### GR?FICA DE DOS EJES Y ###############
library(latticeExtra)

data <- data.frame(database$hrsocup,database$ingocup/1000)
colnames(data)<-c("Horas","Ingreso")

plot(database$hrsocup ~ database$ingocup,
     xlab = "Ingreso",
     ylab = "Horas Laborales",
     main = "Dispersion",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##########################################

HorasPoisson <-glm(formula = Horas ~ Ingreso,
                    data, family = poisson)

lm.fit <-  lm(Horas ~ Ingreso,data)

# PARA ANALIZAR AMBAS REGRESIONES
#install.packages("stargazer")
library(stargazer)
stargazer(HorasPoisson,lm.fit,
          type = "text",
          align=TRUE,
          title="Comparativo") 


