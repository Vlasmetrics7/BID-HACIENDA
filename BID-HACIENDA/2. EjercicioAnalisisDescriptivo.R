
# EJERCICIO

# Usemos la base de datos ya cargada en R "mtcars"

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars

# carguemos la base
attach(mtcars)

# La base presenta 11 características de 32 automóviles  

# Pregunta 1
# ¿Cuál será las millas/gallon (mpg) promedio de los vehículos de acuerdo al
# número de cilindros?

tapply(mpg, cyl, mean)

# Pregunta 2
# Obtener los estadísticos descriptivos de todas las variables 

(summary(mtcars))

# Pregunta 3
# ¿Cómo se distribuirá el tiempo que tardan en alcanzar el 1/4 de milla?

hist(mtcars$qsec,
     xlab = "Segundos", ylab = "Frecuencia",
     main = "Histograma del tiempo al 1/4 de milla",
     breaks = 8,
     col = "dodgerblue",
     border = "black")

# Pregunta 4
# Obtener matrices de dispersión

# forma 1
pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Matriz básica de dispersión")


# forma 2
#install.packages("gclus")
library(gclus)
dta <- mtcars[c(1,3,5,6)]
dta.r <- abs(cor(dta)) # correlaciones
dta.col <- dmat.color(dta.r) # colores
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables ordenadas y coloreadas por su correlación" )

# forma 3
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(wt,disp,mpg, main="3D Scatterplot")

scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

# forma 4
#install.packages("rgl")
library(rgl)
plot3d(wt, disp, mpg, col="red", size=3)



