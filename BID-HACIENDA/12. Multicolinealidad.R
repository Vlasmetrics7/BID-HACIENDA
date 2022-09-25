# MULTICOLINEALIDAD

# Exploremos cómo detectar la multicolinealidad imperfecta
# tenemos datos de las temperaturas máximas para Madrid, Barcelona, Sevilla y La Coruña

library(readxl)
TempMaxSpain <- read_excel("TempMaxSpain.xlsx")
TempMaxSpain <- TempMaxSpain[,2:5]

TempMax<- ts(TempMaxSpain,frequency=12,start=c(1901,1))
plot(TempMax)

# Supongamos que queremos estimar Madrid en función de las demás

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

# corramos la regresión
ModReg<- lm(Madrid~Barcelona+Sevilla+Coruna)

#install.packages('olsrr')
library(olsrr)

####################
# Índice de condición
####################

# Las combinaciones lineales son elegidas tales que la primera tenga
# la varianza más grande [el eigenvalue mayor] (sujeta a algunas restricciones 
# que no nos son relevantes en este curso). La segunda combinación tiene el 
# segundo eigenvalue mayor sujeto a que no esté correlacionado con el primero. 
# Y así sucesivamente.

# Recordar Regla: Multicolinealidad es encontrada cuando dos o más variables
# tienen proporciones de varianza altas (>0.5) que corresponden como regla
# de dedo a un índice de condición mayor a 30.


ols_eigen_cindex(ModReg)

####################
# Factor de Inflación de la Varianza (FIV) (o VIF, por sus siglas en inglés)
####################

# Recordemos que el FIV mide la inflación en las varianzas de los parámetros
# debido a la colinealidad que existe entre los predictores.


ols_vif_tol(ModReg)


# Diagnóstico General

ols_coll_diag(ModReg)




