"Se estimará una serie de 200 obs, con un término autorregresivo y uno de media móvil, junto con sus coeficientes asociados:"
  
y <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.8, ma = 0.3), n = 200)
plot(y)

"Con la función arima.sim() y las demás vistas en la sesión, realiza lo siguiente:"
    
"1.Simula n = 1000 valores de un proceso ARIMA(1, 1, 1) con parámetros ar = 0.6 y ma = 0.2"
set.seed(3)
x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.6, ma = 0.2), n = 1000)
plot(x)


"2.Ajusta un modelo Arima a la serie simulada para estimar los parámetros y observa las estimaciones de los parámetros"
(fit <- arima(x, order = c(1, 1, 1))) #x[t] = 0.5603*x[t-1]+0.2484*w[t-1]+w[t]


"3.Obtén el correlograma de los residuales del ajuste"
acf(resid(fit), main = "")
title(main = "Correlograma de los residuales del ajuste",
      sub = "Gráfica la correlación de los residuales del ajuste")
  
"4.Realiza tres predicciones con ayuda del modelo ajustado y la función predict()"
(pred <- predict(fit, n.ahead = 3)$pred)
#14.22793, 14.23524, 14.23933