
# Una aplicación a la brecha salarial de género

# La base de datos muestra la brecha salarial de género existente entre
# egresados de posgrados en universidades de Estados Unidos entre 1992 
# a 2008 con precios de 2008

library(readxl)
library(dplyr)

# leemos datos
cps <- read_excel("cps.xlsx")

# Un primer acercamiento a la base de datos

head(cps)

# Podemos agrupar por género y año, y calcular algunos estadísticos
# descriptivos

# Con el operador %>% encadenamos algunas funciones

avgs <- cps %>% 
  group_by(a_sex, year) %>% 
  summarise(mean(ahe08), 
            sd(ahe08), 
            n())

print(avgs)

# Pudiéramos estar interesados en partir la muestra por género
# dplyr::filter   indica que de la librería dplyr tome la función filter

hombre <- avgs %>% dplyr::filter(a_sex == 1) 

mujer <- avgs %>% dplyr::filter(a_sex == 2)


# renombremos columnas 

colnames(hombre)   <- c("Sexo", "Año", "Y_bar_h", "s_h", "n_h")
colnames(mujer) <- c("Sexo", "Año", "Y_bar_m", "s_m", "n_m")

# estimamos la brecha de género y calculamos errores estándar e
# intervalos de confianza

brecha <- hombre$Y_bar_h - mujer$Y_bar_m

brecha_se <- sqrt(hombre$s_h^2 / hombre$n_h + mujer$s_m^2 / mujer$n_m)

brecha_ci_l <- brecha - 1.96 * brecha_se

brecha_ci_u <- brecha + 1.96 * brecha_se

resultados <- cbind(hombre[,-1], mujer[,-(1:2)], brecha, brecha_se, 
                    brecha_ci_l, brecha_ci_u)


print(resultados, digits = 3)


# ¿Conclusiones?







