# Ecuación de salario para mujeres trabajadoras y casadas.

#Instalamos el paquete del libro muy popular:
# Wooldridge, J. M. (2015). Introductory econometrics: A modern approach. Cengage learning.

#install.packages("wooldridge")
library(wooldridge)

# Vamos a usar la base de datos del paper:
# Wooldridge Source: T.A. Mroz (1987), "The Sensitivity of an Empirical Model 
# of Married Women's Hours of Work to Economic and Statistical Assumptions," 
# Econometrica 55, 765-799.

data('mroz')

# Analicemos la siguiente regresión simple
# educ: years of schooling
# lwage: log(wage)
R1<-lm(lwage~educ,data=mroz)
summary(R1)

# Controlando por la edad
# age: woman's age in yrs
R2<-lm(lwage~educ+age,data=mroz)
summary(R2)

# Controlando por la edad y la experiencia
# exper: actual labor mkt exper
# expersq: exper^2
R3<-lm(lwage~educ+age+exper+expersq,data=mroz)
summary(R3)

# Controlando por la edad, la experiencia y número de hijos
#kidslt6: # kids < 6 years
#kidsge6: # kids 6-18
R4<-lm(lwage~educ+age+exper+expersq+kidslt6+kidsge6,data=mroz)
summary(R4)

library(stargazer)
stargazer(R1,R2,R3,R4,title="Results",align=TRUE,no.space=TRUE,type="text")

