"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas
de un producto particular, y el conjunto de datos con el que disponemos son datos de publicidad
que consisten en las ventas de aquel producto en 200 diferentes mercados, junto con presupuestos
de publicidad para el producto en cada uno de aquellos mercados para tres medios de comunicación
diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar directamente
las ventas del producto. Por otro lado, ellos pueden controlar el gasto en publicidad para cada
uno de los tres medios de comunicación. Por lo tanto, si determinamos que hay una asociación entre
publicidad y ventas, entonces podemos instruir a nuestro cliente para que ajuste los presupuestos
de publicidad, y así indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para
predecir las ventas sobre la base de los tres presupuestos de medios de comunicación.
Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo más
adecuado siguiendo los procedimientos vistos

Considera:
  
Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
str(adv)
attach(adv)
pairs(~ Sales + TV + Radio  + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5) #~ signo para hacer relaciones

m1 <- lm(Sales ~ TV + Radio  + Newspaper)
(summary(m1))
StanRes <- rstandard(m1)

par(mfrow = c(2, 2))
plot(TV, StanRes, ylab = "Residuales Estandarizados")
plot(Radio, StanRes, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes, ylab = "Residuales Estandarizados")
qqnorm(StanRes)
qqline(StanRes)
dev.off()

shapiro.test(StanRes) #p-value = 0.001339



m2 <- update(m1, ~.- Radio - Newspaper)
summary(m2)
StanRes2 <- rstandard(m2)

par(mfrow = c(2, 2))
plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes2, ylab = "Residuales Estandarizados")
qqnorm(StanRes2)
qqline(StanRes2)
dev.off()

shapiro.test(StanRes2) #p-value = 0.526



m3 <- update(m1, ~.- TV - Newspaper)
summary(m3)
StanRes3 <- rstandard(m3)

par(mfrow = c(2, 2))
plot(TV, StanRes3, ylab = "Residuales Estandarizados")
plot(Radio, StanRes3, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes3, ylab = "Residuales Estandarizados")
qqnorm(StanRes3)
qqline(StanRes3)
dev.off()

shapiro.test(StanRes3) #p-value = 4.408e-05



m4 <- update(m1, ~.- TV - Radio)
summary(m4)
StanRes4 <- rstandard(m4)

par(mfrow = c(2, 2))
plot(TV, StanRes4, ylab = "Residuales Estandarizados")
plot(Radio, StanRes4, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes4, ylab = "Residuales Estandarizados")
qqnorm(StanRes4)
qqline(StanRes4)
dev.off()

shapiro.test(StanRes4) #p-value = 0.05055


data <- data.frame(
  TV = c(204.5, 186.4, 143.5, 237.4),
  Radio = c(48.9, 35.1, 23.9, 15.9),
  Newspaper = c(11.6, 46.0, 23.5, 7.2),
  Sales = c(16.5, 17.5, 13.2, 22.1)
)

predict(m1, newdata = data, interval = "confidence", level = 0.99) #All
predict(m2, newdata = data, interval = "confidence", level = 0.99) #TV
predict(m3, newdata = data, interval = "confidence", level = 0.99) #Radio
predict(m4, newdata = data, interval = "confidence", level = 0.99) #Newspaper
