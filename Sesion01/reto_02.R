class(netflix_titles)
names(netflix_titles)

#A) Usando la función de lectura adecuada, lee el archivo que se encuentra en la siguiente url
netflix <- read.csv(file = 'netflix_titles.csv')


head(netflix)
class(netflix)
str(netflix)


#B) Verifica la clase de la tabla, el número de variables y de observaciones, así como el nombre de las variables
dim(netflix)#primer número son observaciones(filas), segundo número variables(atributos)
names(netflix)#lista las variables(atributos)


"documentación seleccionar filas de un dataframe
https://sparkbyexamples.com/r-programming/select-rows-in-r/
"
#C) Usando indexación, selecciona sólo los datos de películas que fueron estrenadas desde 2015 y hasta antes del 2019
net.2015 <- netflix[netflix$release_year >= 2015 & netflix$release_year < 2019,]
net.2015

#D) Por último, averigua que hace la siguiente función
View(net.2015) #crea una vista en tabla del objeto
View(netflix)