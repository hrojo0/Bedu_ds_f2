library(ggplot2)
library(dplyr)
library(DescTools)

str(diamonds)

#Calcula e interpreta las medidas de tendencia central de la variable price, datos atípicos en alguno de los extremos de los datos
mean(diamonds$price)#Promedio: 3932.8
median(diamonds$price)#Mediana: 2401
Mode(diamonds$price)[1]#Moda: 605


"Con base en tu resultado anteior, ¿qué se puede concluir respecto al sesgo del precio?
datos atípicos en alguno de los extremos de los datos, algunos datos estan dispersos,
además se encuentra un sesgo a la derecha por lo que los datos se concentran al inicio del histograma
debido a que cumple la condicion moda < mediana < media"


#Calcula e interpreta la desviación estándar y los cuartiles de la distribución:
sd(diamonds$price)#Desviación estándar: 3989.44
(cuartiles <- quantile(diamonds$price, probs = c(.25, .5, .75)))

  
#Realiza un histograma de la variable precio. ¿Su distribución coincide con tu conclusión de la pregunta 2?
k = ceiling(sqrt(length(diamonds$price)))
min.price <- min(diamonds$price)
max.price <- max(diamonds$price)
ac = (max.price - min.price)/k*2

clases <- seq(min.price, max.price, by = ac)

bins <- seq(min(diamonds$price), max(diamonds$price), by = ac)

?hist
histograma <- hist(diamonds$price, breaks = 20)

price.diamonds <- ggplot(diamonds, aes(price)) +
       geom_histogram(bins = 20, binwidth = 1000, aes(fill=after_stat(count)), col = "black") + 
       labs(title = "Precios de diamantes", x = "Precio", y = "Frequency") + 
       theme_light()

ggsave("Precio de Diamantes.jpg", plot = price.diamonds)


#Realiza un boxplot de la variable precio. ¿Su forma se relaciona con la distribución mostrada por el histograma anteior? ¿Existen datos atípicos?
ggplot(diamonds, aes(price)) +
      geom_boxplot()


head(ToothGrowth)
head(diamonds)

ggplot(ToothGrowth, aes(x=dose, y=len, group = supp)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

ggplot(diamonds, aes(price, group = cut)) + 
       geom_boxplot(outlier.shape=5,outlier.size=4) +
       coord_flip()

       