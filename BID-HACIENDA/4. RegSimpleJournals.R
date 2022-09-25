# PRIMER EJERCICIO DE REGRESIÓN SIMPLE

# Los datos vienen del libro de Stock and Watson (2007) 
# Los datos dan informaciÓn acerca de las subscripciones a journal de economía/finanzas
# por bibliotecas de los EUA en el año 2000

#install.packages("AER")
library(AER)
#lectura de datos
data("Journals")

# vamos a elegir solo dos variables para hacer el ejercicio
journals <- Journals[, c("subs", "price")]

# Definamos la siguiente variable
# ¿Qué interpretación tendrá?
journals$citeprice <- Journals$price/Journals$citations
# estadísticos de resumen
summary(journals)

# Nos interesa estimar el siguiente modelo:
# ¿cuál será su interpretación?
# log(subs) = b1 + b2 log(ratio) + u

# podemos calcular logaritmos porque tenemos que el rango de las variables
# es muy grande y tienen una cantidad considerable de sesgo

plot(log(subs) ~ log(citeprice), data = journals)

# en este ejercicio el objetivo es estimar el efecto que tiene el precio
# entre citas en el número de suscripciones de las bibliotecas

# el comando lm es "linear model", estima nuestro modelo de regresión
# abline solo agrega la línea de regresión
# log(subs) = b1 + b2 log(ratio) + e #

jour_lm <- lm(log(subs) ~ log(citeprice), data = journals)
abline(jour_lm)

# ANálisis de regresión simple

# print()     imprime los resultados de forma muy simple

print(jour_lm)

# summary()   resultados estándar de regresión

summary(jour_lm)

# coef() (or coefficients()) extrae los coeficientes de regresión

coef(jour_lm)
coefficients(jour_lm)

# Si queremos anazlizar con detalles los coeficientes

jour_slm <- summary(jour_lm)
jour_slm$coefficients
pval<-jour_slm$coefficients
pval[,4]

# residuals() (or resid()) extraer residuales
# fitted() (or fitted.values()) extraer valores ajustados
# anova()     análisis anova
# predict()   predicciones
# plot()      gráficas de diagnóstico
# confint()   intervalos de confianzaa de los coeficientes de regresión
# deviance()  Suma de cuadrados de los residuales
# vcov() (estimated) matriz de covariznzas de los parámetros
# logLik()    log-likelihood (asumiendo que los errores son normales)
# AIC()       Criterio de información de Akaike (asumiendo que los errores son normales)

#Analysis of variance ANOVA
anova(jour_lm)

#  Estimaciones puntuales y por intervalos
coef(jour_lm)
confint(jour_lm, level = 0.95)

# Predicciones

# Hay dos tipos de predicciones. 
# Los errores estandars de las predicciones fuera de muestra
# consideran tanto la incertidumbre en la regresión como la variación
# de los puntos individuales sobre la línea.
# Entonces, los intervalos de predicción fuera de muestra son más grandes
# que las predicciones dentro de muestra.


lciteprice <- seq(from = -6, to = 4, by = 0.25)
jour_pred <- predict(jour_lm, interval = "prediction",
                     newdata = data.frame(citeprice = exp(lciteprice)))
plot(log(subs) ~ log(citeprice), data = journals)
lines(jour_pred[, 1] ~ lciteprice, col = 1)
lines(jour_pred[, 2] ~ lciteprice, col = 1, lty = 2)
lines(jour_pred[, 3] ~ lciteprice, col = 1, lty = 2)


# Pruebas de hipótesis
# H0: B2 = -0.5

linearHypothesis(jour_lm, "log(citeprice) = -0.5")
