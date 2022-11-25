set.seed(0202)

#a) Grafica la función de distribución de la variable aleatoria. (Asume que se obtienen 10,000 muestras)
binom <- rbinom(n = 10000, size = 10, prob = 0.15)

barplot(table(binom)/length(binom),
        main = "Prob de ensamblar pieza defectuosa", 
        xlab = "# de unidades defectuosas")


#b) ¿Cuál es la probabilidad de que se produzcan dos unidades defectuosas? 27.58%... dbinom P(X) = x 
dbinom(x = 2, size = 10, prob = 0.15)


#c) ¿Cuál es la probabilidad de que a lo mucho 4 unidades sean defectuosas? 99.01%
pbinom(q = 4, size = 10, prob = 0.15, lower.tail = TRUE) 


#d) ¿Cuál es la probabiliad de que por lo menos tres unidades se encuentren defectuosa? 17.98%
pbinom(q = 2, size = 10, prob = 0.15, lower.tail = FALSE) 


#e) ¿Cuál es la probabilidad de que entre 2 y 4 unidades se encuentren defectuosas? 16.99%
pbinom(4, 10, 0.15) - pbinom(1, 10, 0.15) 

dbinom(x = 2, size = 10, prob = 0.15) +
  dbinom(x = 3, size = 10, prob = 0.15) +
  dbinom(x = 4, size = 10, prob = 0.15) 
#P(X <= b) - P(X <= a) =  P(a < X <= b)


#f) ¿Cuál es el número esperado de unidades defectuosas? 1.4901 pzas ¿Con qué variación? 1.1353
mean(binom)
sd(binom)
