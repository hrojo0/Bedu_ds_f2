library(ggplot2)
library(dplyr)
View(diamonds)
var2 <- diamonds

"¿Qué tipo de variable y escala de medición tiene la variable cut? ordered factr = cualitativa ordinal"
class(var2$cut)

"Reliza una tabla de frecuencias absolutas y relativas"
summary(var2)
freq2 <- table(var2$cut)
freq2
transform(freq2,
          Freq.Relativa = round(prop.table(Freq),2))

"Para esta variable, ¿es posible calcular la frecuencia relativa acumulada? En caso afirmativo, agrégala a tu tabla anterior"
tabla.completa <- transform(freq2,
          Freq.Relativa = prop.table(Freq),
          Freq.Acum = cumsum(prop.table(Freq)))

tabla.completa
"Con base en tu tabla, responde:
¿Cuál es el porcentaje de diamantes que tienen un corte Very Good? 22.4%"
round(tabla.completa$Freq.Relativa[3]*100,2)

#¿Cuál es el porcentaje de diamantes que tienen un corte entre Fair y Very Good? 34.48%
round(sum(tabla.completa$Freq.Relativa[1:3])*100,2)

#¿Cuál es el porcentaje de diamantas que tienen un corte al menos Very Good? 87.92%
round(sum(tabla.completa$Freq.Relativa[3:5])*100,2)


"Crea una tabla de distribución de frecuencias para el precio de los diamantes.
Para ello determina el número de clases con base en la regla de Sturges k = 1+3.3Log10(n)"
k = ceiling(1+3.3*log10(length(var2$price)))
ac = (max(var2$price) - min(var2$price)) / k

bins <- seq(min(var2$price), max(var2$price), by = ac)

price.diamonds <- cut(var2$price, breaks = bins, include.lowest = TRUE, dig.lab = 10)

dist.freq <- table(price.diamonds)

tabla.sturges <- transform(dist.freq,
          Freq.Relativa = prop.table(Freq),
          Freq.Acumulada = cumsum(prop.table(Freq)))


#Con base en tu tabla, responde:
tabla.sturges
#¿Cuál es el porcentaje de diamantes que tienen un precio entre 3590.17 y 4678.23? 9.31%
round(tabla.sturges$Freq.Relativa[4]*100,2)

#¿Cuál es el porcentaje de diamantes que tienen un precio menor a 7942.41? 85.75%
round(tabla.sturges$Freq.Acumulada[7]*100,2)

#¿Cuál es el porcentaje de diamantes que tienen un precio mayor a 11206.58? 7.54%
round(sum(tabla.sturges$Freq.Relativa[11:length(tabla.sturges$Freq.Acumulada)])*100,2)
