# VISUALIZANDO REGRESION LINEAL

# install.packages("gcookbook")
# Usamos una base de datos del libro "R Graphics Cookbook" de Winston Chang, 
# publicado por O'Reilly Media.

#install.packages("ggplot2")
library(ggplot2)
library(gcookbook)  

# Usamos la base de datos:  Height and weight of schoolchildren
# Clark, T., Sydenstricker, E., & Collins, S. D. (1922). 
# Heights and Weights of School Children. Public Health Reports (1896-1970), 1185-1207.

hw_sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn))

hw_sp +
  geom_point() +
  stat_smooth(method = lm)

# por default, stat_smooth() impone la región de confianza al 95% en la recta de regresión. 

# Lo podemos cambiar al 99%
hw_sp +
  geom_point() +
  stat_smooth(method = lm, level = 0.99)

# o simplemente elimnamos bandas si no nos gusta
hw_sp +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

# podemos cambiar a nuestros colores favoritos
hw_sp +
  geom_point(colour = "grey60") +
  stat_smooth(method = lm, se = 0.95, colour = "black")

# A veces es importante partir nuestras muestras:
# Analizando los datos si están agrupados mediante un "factor"

hw_sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1")

hw_sp +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

#hay varias opciones de estimación: "lm", "glm", "loess"



############## Visualizando residuales ###########

### aquí puede seguir algunos ejemplos
###  https://rpubs.com/mglantz/690906

fit <- lm(heightIn ~ ageYear, data = heightweight)
heightweight$predicted <- predict(fit)   # Guardamos los valores ajustados
heightweight$residuals <- residuals(fit) # Guardamos los residuales


ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = ageYear, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +  
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  
  guides(scale = "none") +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

