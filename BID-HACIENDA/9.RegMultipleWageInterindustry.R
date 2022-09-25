# Ecuación de salario para mujeres trabajadoras y casadas.

#Instalamos el paquete del libro muy popular:
# Wooldridge, J. M. (2015). Introductory econometrics: A modern approach. Cengage learning.

#install.packages("wooldridge")
library(wooldridge)

# Vamos a usar la base de datos del paper:
# M. Blackburn and D. Neumark (1992), "Unobserved Ability, Efficiency Wages, 
# and Interindustry Wage Differentials," Quarterly Journal of Economics 107, 1421-1436. 

data('wage2')

# Analicemos la siguiente regresión simple
# educ: years of schooling
# lwage: log(wage)
R1<-lm(lwage~educ,data=wage2)
summary(R1)

# Controlando por la experiencia
# exper: years of work experience
# tenure: years with current employer
R2<-lm(lwage~educ+exper+tenure,data=wage2)
summary(R2)

# Controlando por estado civil casado
# married: =1 if married
R3<-lm(lwage~educ+exper+tenure+married,data=wage2)
summary(R3)

# Controlando por lugar de residencia
# south: =1 if live in south
# urban: =1 if live in SMSA
R4<-lm(lwage~educ+exper+tenure+married+south+urban,data=wage2)
summary(R4)

# Controlando por IQ
# IQ: IQ score
R5<-lm(lwage~educ+exper+tenure+married+south+urban+IQ,data=wage2)
summary(R5)



library(stargazer)
stargazer(R1,R2,R3,R4,R5,title="Results",align=TRUE,no.space=TRUE,type="text")