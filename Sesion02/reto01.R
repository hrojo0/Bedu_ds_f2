library(dplyr)

install.packages("dplyr")

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"


#Crea una lista con los nombres de las variables anteriores
lista.archivos <- list(u1011, u1112, u1213, u1314)


#Con ayuda de la función lapply() lee todos los .csv de lista.archivos.
#Usa help('lapply') para saber más de esta función. ¿Qué tipo de objeto regresa lapply?
?lapply
lista.csv <- lapply(lista.archivos, read.csv, header = TRUE)
View(lista.csv)

#Ejecuta la siguiente función para sólo tomar un rango de variables. Más adelante en esta sesión veremos la función select()
lista.datos <- lapply(lista.csv, select, Date:FTR) #selecciona cols desde DATE hasta FTR
df <- do.call(rbind, lista.datos)
head(df)

#Utiliza la función do.call() para repetir una función que te permite juntar por filas todos los elementos de lista.datos.
#Usa help('do.call') para saber más de esta función.
?do.call
df <- do.call(rbind, lista.datos)
head(df)
View(df)
