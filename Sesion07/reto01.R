"1. Coloca el número diez como semilla"
set.seed(10)
par(mfrow = c(2, 3))

"2. Simula un proceso AR(1) de la forma x[t] = 0.8 * x[t-1] + w[t] para t = 1, 2, ..., 200
y muestra gráficamente la serie de tiempo obtenida"
x <- w <- rnorm(200)
for(t in 2:200) x[t] <- 0.8 * x[t-1] + w[t]

plot(x, type = "l", main = "Caminata Aleatoria Simulada", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==0.8*x[t-1]+w[t]))


"3. Obtén el correlograma y el correlograma parcial del proceso AR(1) simulado"
acf(x)
pacf(x)


"4. Ajusta un modelo autorregresivo a la serie simulada utilizando la función ar,
observa el orden del modelo y los parámetros estimados. ¿Coinciden con el modelo original?"
plot(diff(x), type = "l", main = "Primera diferencia de X", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==0.8*x[t-1]+w[t]))

acf(diff(x))
pacf(diff(x))
ar(x,method = "mle")

dev.off()
