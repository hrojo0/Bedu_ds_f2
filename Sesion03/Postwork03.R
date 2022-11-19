# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo
library(dplyr)
library(ggplot2)
library(DescTools)

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

df <- na.omit(df)

df$Categoria <- factor(df$Categoria)
df$Grupo <- factor(df$Grupo, labels = c("No", "Si"))

summary(df)

#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`
mean(df$Mediciones); median(df$Mediciones); Mode(df$Mediciones)[1]
"Con los resultados de las medidas de tendencia central se puede observar dispersión en los datos"



#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
"El histograma resultante tendrá un sesgo a la derecha dado el criterio Mode<Mediana<Media
por lo que la mayoría de los datos se concentrarán al lado izquierdo del histograma"



#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
(desv.est <- sd(df$Mediciones)); min(df$Mediciones); max(df$Mediciones)
#Los datos se encuentran dispersos con concentración de frecuencia en los datos de menor tamaño

(cuartiles <- quantile(df$Mediciones, probs = c(.25, .5, .75)))
#Cuartiles 25% = 23.45, 50% = 49.3, 75% = 82.85



"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"
(hist.mediciones <-
      ggplot(df, aes(x = Mediciones)) +
      geom_histogram(aes(color = Categoria, fill = Categoria), position = "identity", bins = 30, alpha = 0.4) +
      scale_color_manual(values = c("#97DB4F", "#E55381", "#0075C4")) +
      scale_fill_manual(values = c("#97DB4F", "#E55381", "#0075C4")) +
      geom_vline(aes(xintercept = mean(Mediciones)), color = "#D16014", linetype = "dashed", size = .5) + 
      labs(title = "Mediciones por Categoría",x = "Mediciones", y = "Frecuencia") + 
      theme_light())

"Las 3 categorías están generando el sesgo debido a que cada una los datos se concentran en los datos de menor valor"
ggsave("Histograma - Mediciones por Categoría.jpg", plot = hist.mediciones)


"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"
(boxplot.mediciones <- 
      ggplot(df, aes(Mediciones, x = Mediciones, y = Categoria, fill = Grupo)) + 
      geom_boxplot(outlier.size = 3, outlier.colour = "#E55381", outlier.fill = "#E55381", outlier.alpha = 0.6, outlier.shape = 18) +
      labs(title = "Mediciones por Categoría",x = "Mediciones", y = "Categoría") + 
      coord_flip() + 
      theme_light())
ggsave("Boxplot - Mediciones por Categoria.jpg", plot = boxplot.mediciones)

#Para poner color con en los plots.... boxplot.mediciones+scale_fill_manual(values=c("#F58F29", "#97DB4F"))
"Existen diferencias entre las categorías en cuestión de los Grupos ya que el Grupo 'Si' se mantiene en un rango más estable de mediciones,
mientras que el Grupo 'No' se acrecenta en cada grupo lo que genera que haya una diferencia notoria entre los grupos de la Categoría 3
siendo el grupo 'Si' el que cuenta con menores mediciones lo que provoca que se genere el sesgo de los datos"

