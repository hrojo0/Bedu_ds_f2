set.seed(0202)

"Un banco recibe, en promedio, 6 cheques sin fondo por día"

#a) Grafica la función de distribución de la variable aleatoria. (Asume que se obtienen 10,000 muestras)
poisson <- rpois(n = 10000, lambda = 6)
barplot(table(poisson)/length(poisson),
        main = "Distribución de Poisson", 
        xlab = "X=x")


#b) ¿Cuál es la probabilidad de que reciba 4 cheques sin fondo en un dia? 13.38%
dpois(4,6)


#c) ¿Cuál es la probabilidad de que reciba más de 8 cheques sin fondo? 15.27%
ppois(8,6, FALSE)

 
#d) ¿Cuál es la probabilidad de que reciba entre 4 y 10 cheques sin fondo? 67.23%
ppois(10, 6) - ppois(4, 6)

 
#e) ¿Cuál es la probabilidad de que tengan que pasar 5 horas o menos hasta que se presente el siguiente cheque sin fondos? 71.34%
pexp(q = 5, rate = 6/24)

 
#f) ¿Cuál es la probabilidad de que tengan que pasar entre 2 y 4 hasta que se presente el siguiente cheque sin fondos? 23.86%
pexp(4, 6/24) - pexp(2, 6/24) 

 
#g) Realiza la gráfica de distribución de probabilidad de la variable aleatoria anterior
curve(dexp(x, rate = 6/24), from=0, to=30, 
      col='blue', main = "Distribución exponencial",
      ylab = "f(x)", xlab = "Tiempo entre eventos")
