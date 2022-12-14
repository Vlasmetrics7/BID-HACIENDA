# MULTICOLINEALIDAD

# Exploremos c?mo detectar la multicolinealidad imperfecta
# tenemos datos de las temperaturas m?ximas para Madrid, Barcelona, Sevilla y La Coru?a

library(readxl)
TempMaxSpain <- read_excel("TempMaxSpain.xlsx")
TempMaxSpain <- TempMaxSpain[,2:5]

TempMax<- ts(TempMaxSpain,frequency=12,start=c(1901,1))
plot(TempMax)

# Supongamos que queremos estimar Madrid en funci?n de las dem?s

library(dplyr)
TempMax[,2:4] %>% cor(method="pearson") %>% round(digits=2) -> TempMax_cor
TempMax_cor

library(GGally)
DataTemp<-data.frame(TempMax[,2:4])
ggpairs(DataTemp, title="Correlaciones") 

Madrid<-TempMaxSpain$Madrid
Barcelona<-TempMaxSpain$Barcelona
Sevilla<-TempMaxSpain$Sevilla
Coruna<-TempMaxSpain$`A Coruna`

# corramos la regresi?n
ModReg<- lm(Madrid~Barcelona+Sevilla+Coruna)

#install.packages('olsrr')
library(olsrr)

####################
# ?ndice de condici?n
####################

# Las combinaciones lineales son elegidas tales que la primera tenga
# la varianza m?s grande [el eigenvalue mayor] (sujeta a algunas restricciones 
# que no nos son relevantes en este curso). La segunda combinaci?n tiene el 
# segundo eigenvalue mayor sujeto a que no est? correlacionado con el primero. 
# Y as? sucesivamente.

# Recordar Regla: Multicolinealidad es encontrada cuando dos o m?s variables
# tienen proporciones de varianza altas (>0.5) que corresponden como regla
# de dedo a un ?ndice de condici?n mayor a 30.


ols_eigen_cindex(ModReg)

####################
# Factor de Inflaci?n de la Varianza (FIV) (o VIF, por sus siglas en ingl?s)
####################

# Recordemos que el FIV mide la inflaci?n en las varianzas de los par?metros
# debido a la colinealidad que existe entre los predictores.


ols_vif_tol(ModReg)


# Diagn?stico General

ols_coll_diag(ModReg)




