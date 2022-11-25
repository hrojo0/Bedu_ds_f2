set.seed(0202)

"1.- El tiempo necesario para producir un determinado producto en una maquinaria tiene una distribución normal
cuya media es 80 minutos con desviación estándar de 10 minutos."

#a) Grafica la función de distribución de la variable aleatoria.
curve(dnorm(x, mean = 80, sd = 10), from = 30, to = 130, 
      col='blue', main = "Densidad Normal:\nDiferente media",
      ylab = "f(x)", xlab = "tiempo necesario de producción (minutos)")

#b) ¿Cuál es la probabilidad de que la maquinaria termine el producto en una hora o menos? 2.27%
pnorm(60, 80, 10)

 
#c) ¿Cuál es la probabilidad de que el producto sea terminado en más de 1.5 horas, pero en menos de 2.5 horas? 15.86%
pnorm(150, 80, 10) - pnorm(90, 80, 10)


#d) ¿Cuál es el intervalo de tiempo que deja exactamente al centro el 90% de probabilidad? de 63.55 a 96.44
qnorm(p = 0.05, 80, 10); qnorm(p = 0.05, 80, 10, FALSE) 
#qnorm = cuantiles



"2.- Una institución de crédito informa a las autoridades que, en promedio, sus clientes mantienen un saldo deudor
en sus tarjetas de crédito igual a 12,500 pesos mensuales, con una desviación estándar de 7,800 pesos."

#a) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor mayor a $20,000? 16.81%
pnorm(20000, 12500, 7800, FALSE)

  
#b) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor menor a $11,000? 42.37%
pnorm(11000, 12500, 7800)
 
 
#c) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor entre $13,000 y 15,000? 10.01%
pnorm(15000, 12500, 7800) - pnorm(13000, 12500, 7800)
  

#d) ¿Hasta qué monto se encuentra el 10% de clientes con saldo deudor más bajo? $2503.898
qnorm(0.1, 12500, 7800)
