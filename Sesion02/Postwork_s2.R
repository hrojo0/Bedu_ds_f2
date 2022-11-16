# Postwork Sesión 2.

#### Objetivo

"- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`
#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión."

#### Desarrollo

"1) Inspecciona el DataSet iris disponible directamente en la librería de ggplot. 
Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes y 
que los datos se encuentran listos para usarse."
?iris
iris.df <- iris
str(iris)
complete.cases(iris.df)


"2) Crea una gráfica de puntos que contenga `Sepal.Lenght` en el eje horizontal, 
`Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño 
de la figura está representado por `Petal.Width`. 
Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`."

library(ggplot2)
graph.1 <- ggplot(iris.df, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Width)) + geom_point(shape = 10, alpha = 0.5)
graph.1
ggsave("Iris.jpg", plot = graph.1)

"3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas las variables 
agrupadas por `Species`."
library(dplyr)
iris.mean <- iris.df %>%
             group_by(Species) %>%
             summarize(Sepal.Length = mean(Sepal.Length),
                       Sepal.Width = mean(Sepal.Width),
                       Petal.Length = mean(Petal.Length),
                       Petal.Width = mean(Petal.Width))
iris.mean


"4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, 
`fill = 'black'` y `stroke = 2`. También agrega etiquetas, temas y los cambios 
necesarios para mejorar tu visualización."
graph.2 <- graph.1 + geom_point(data = iris.mean, shape = 23, alpha = 4, fill = "black", stroke = 2)
graph.2 <- graph.2 + scale_y_continuous(limits = c(1.5,4.5), breaks = seq(1.5, 4.5, 0.25)) +
                     scale_color_discrete("Especie", labels = c("Setosa", "Versicolor", "Virginica")) +
                     scale_size("Petalos", labels = c("0 pétalos","0.5 pétalos","1 pétalo","1.5 pétalos","2 pétalos","2.5 pétalos")) +
                     labs(title = "Tipos de Plantas Iris", x = "Longitud del sépalo", y = "Ancho del sépalo") +
                     theme_classic()
graph.2
ggsave("Iris_Diseño.jpg", plot = graph.2)
