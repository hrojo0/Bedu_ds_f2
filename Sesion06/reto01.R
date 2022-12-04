dfr1 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/SwedishMotorInsurance.csv", header = TRUE)
head(dfr1)

"1. Selecciona solo las variables de interés y realiza una matriz de correlaciones."
dfr1.select <- select(dfr1, Claims, Insured, Payment)
head(dfr1.select)

round(cor(dfr1.select),4)  

pairs(~ Claims + Insured + Payment, 
      data = dfr1, gap = 0.4, cex.labels = 1.5) #~ signo para hacer relaciones

"2. Estima un modelo de regresión lineal de acuerdo con lo solicitado por la aseguradora.
No olvides interpretar tus resultados y realizar el diagnóstico sobre los residuos."
attach(dfr1)
m1r1 <- lm(Payment ~ Claims + Insured)
(summary(m1r1))
#Rechazamos Ho para ambas variables independientes
StanRes3 <- rstandard(m1r1)

par(mfrow = c(2, 2))

plot(m1r1$fitted.values, StanRes3, ylab = "Valores ajustados")
plot(Claims, StanRes3, ylab = "Residuales Estandarizados")
plot(Insured, StanRes3, ylab = "Residuales Estandarizados")

qqnorm(StanRes3)
qqline(StanRes3)

dev.off()

shapiro.test(StanRes3)


"3. Con el primero modelo, estima uno nuevo quitando la variable Insured.
No olvides interpretar tus resultados y realizar el diagnóstico sobre los residuos."
m2r1 <- update(m1r1, ~.-Insured)
summary(m2r1)
#Rechazamos Ho para ambas variables independientes
StanRes4 <- rstandard(m2r1)

par(mfrow = c(2, 2))

plot(m2r1$fitted.values, StanRes4, ylab = "Valores ajustados")
plot(Claims, StanRes4, ylab = "Residuales Estandarizados")
plot(Insured, StanRes4, ylab = "Residuales Estandarizados")

qqnorm(StanRes4)
qqline(StanRes4)

dev.off()

shapiro.test(StanRes4)


"4. Con el primero modelo, estima uno nuevo quitando la variable Claims.
No olvides interpretar tus resultados y realizar el diagnóstico sobre los residuos."
m3r1 <- update(m1r1, ~.-Claims)
summary(m3r1)
#Rechazamos Ho para ambas variables independientes
StanRes5 <- rstandard(m3r1)

par(mfrow = c(2, 2))

plot(m3r1$fitted.values, StanRes5, ylab = "Valores ajustados")
plot(Claims, StanRes5, ylab = "Residuales Estandarizados")
plot(Insured, StanRes5, ylab = "Residuales Estandarizados")

qqnorm(StanRes5)
qqline(StanRes5)

dev.off()

shapiro.test(StanRes5)

"5. ¿Cuál de los 3 modelos tiene un mejor poder predictivo?"
#el primer modelo dado que el R-squared es más cercano a la realidad