"El data frame iris contiene información recolectada por Anderson sobre 50 flores de 3 especies distintas
(setosa, versicolor y virginca), incluyendo medidas en centímetros del largo y ancho del sépalo así como de los pétalos."
head(iris)
View(iris)

"Estudios recientes sobre las mismas especies muestran que:
I. En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm"
#Ho: mu = 5.7
#Ha: mu != 5.7
t.test(x = iris[iris$Species == "setosa", "Sepal.Length"], alternative = "two.sided", mu = 5.7)#p-value = 2.2e-16
p.value <- 2.2e-16
if(p.value < 0.01) {
  paste("NC 99%: Existe evidencia para rechazar Ho, el largo del sépalo en promedio no es de 5.7 cm")
} else{
  paste("NC 99%: No existe evidencia para rechazar Ho, el largo del sépalo en promedio es de 5.7 cm")
}


"II. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm"
#Ho: mu >= 2.1
#Ha: mu < 2.1
t.test(x = iris[iris$Species == "virginica", "Petal.Width"], alternative = "less", mu = 2.1)#p-value = 0.03132
p.value <- 0.03132
if(p.value < 0.01) {
  paste("NC 99%: Existe evidencia para rechazar Ho, el ancho del pétalo en promedio es mayor a 2.1 cm")
} else{
  paste("NC 99%: No existe evidencia para rechazar Ho, el ancho del pétalo en promedio es menor a 2.1 cm")
}


"III. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio
del largo del pétalo de la especie versicolor."
var.test(iris[iris$Species == "virginica", "Petal.Length"],
         iris[iris$Species == "versicolor", "Petal.Length"],
         ratio = 1, alternative = "two.sided")

"Planteamiento de hipótesis:
Ho: prom_customer_service_calls_churn1 <= prom_customer_service_calls_churn2 
Ha: prom_customer_service_calls_churn1 > prom_customer_service_calls_churn2"

t.test(x = iris[iris$Species == "virginica", "Petal.Length"],
       y = iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = FALSE) #p-value = 0.03206

p.value <- 0.03206
if(p.value < 0.01) {
  paste("NC 99%: Existe evidencia para rechazar Ho, el ancho del pétalo virginica no es 1.1cm más grande que versicolor")
} else{
  paste("NC 99%: No existe evidencia para rechazar Ho, el ancho del pétalo virginica es 1.1cm más grande que versicolor")
}


"IV. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."
#Ho: prom_setosa_sepal.width = prom_versicolor_sepal.width = prom_virginica_sepal.width
#Ha: Al menos uno es diferente.

boxplot(Sepal.Width ~ Species,
        data = iris)

summary(aov(Sepal.Width ~ Species,
            data = iris))
?aov
