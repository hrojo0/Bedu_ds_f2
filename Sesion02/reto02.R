"Utilizando el DataFrame del Reto01 de esta sesión, crea una tabla 
que muestre sólo aquellos equipos que, en total de la liga, hayan metido 
más del 85% de los goles jugando como local. Muestra sólo las variables
HomeTeam y tu variable de proporción y arregla los datos de forma descendente 
respecto a la proporción de goles"

library(dplyr)
library(ggplot2)

#OBtener los goles como local de cada equipo
home.goals <- df %>%
            group_by(HomeTeam) %>%
            summarize(goles.local = sum(FTHG))
head(home.goals)

#Obtener los goles como visitante de cada equipo
away.goals <- df %>%
  group_by(AwayTeam) %>%
  summarize(goles.visita = sum(FTAG))
head(away.goals)

#Obtener una tabla de goles como local y visita y el total de goles de cada equipo
resumen.goles <- cbind(home.goals, away.goals)
as.data.frame(resumen.goles)
#rownames(resumen.goles) <- resumen.goles[,1]
resumen.goles[,3] <- NULL
resumen.goles.prob <- resumen.goles %>%
                mutate(FTTOT = goles.local + goles.visita, PHG = round(goles.local / FTTOT,2)) %>%
                select(HomeTeam, goles.local, goles.visita, FTTOT, PHG) %>%
                arrange(desc(PHG)) %>%
                filter(PHG >= 0.60)
resumen.goles.prob

"Ningún equipo cumple con lo planteado en el problema ya que la probabilidad máxima
de que un equipo anote más de local que de visitante es de 75% por lo que se ajustó
ese la probabilidad planteada a mínimo 60% de probabilidad de que anote gol como local"

#FTTOT = Total de goles
#PHG = Probabilidad Home Goals