# Postwork Sesión 1.

#### Objetivo

"El Postwork tiene como objetivo que practiques los comandos básicos aprendidos 
durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. Recuerda 
que la programación es como un deporte en el que se debe practicar, habrá caídas, 
pero lo importante es levantarse y seguir adelante. Éxito"

#### Requisitos
#- Concluir los retos
#- Haber estudiado los ejemplos durante la sesión

#### Desarrollo

"El siguiente postwork, te servirá para ir desarrollando habilidades como si se 
tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, 
sesión a sesión se irá desarrollando.
A continuación aparecen una serie de objetivos que deberás cumplir, es un ejemplo 
real de aplicación y tiene que ver con datos referentes a equipos de la liga española 
de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en 
este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar 
análisis interesantes que se pueden aplicar a otras áreas. Siendo así damos paso a las instrucciones:" 
  
"1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de la primera división
de la liga española: https://www.football-data.co.uk/spainm.php"


#2. Importa los datos a R como un Dataframe. NOTA: No olvides cambiar tu dirección de trabajo a la ruta donde descargaste tu archivo
sp1 <- read.csv("SP1.csv")
View(sp1)

"3. Del dataframe que resulta de importar los datos a `R`, extrae las columnas que contienen
los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados
por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados"
goles.local <- sp1$FTHG
goles.visita <- sp1$FTAG

goles.partidos <- sp1[,c("FTHG","FTAG")]
View(goles.partidos)

"4. Consulta cómo funciona la función `table` en `R`. Para ello, puedes ingresar los comandos
`?table` para leer la documentación."
?table
(matriz.goles <- table(goles.partidos$FTHG, goles.partidos$FTAG))
View(matriz.goles)

#5. Responde a las siguientes preguntas:
#  a) ¿Cuántos goles tuvo el partido con mayor empate?
goles.anotados <- 0
goles.mayor.empate <- -1

for(i in 1:length(goles.local)){
  if(goles.local[i] == goles.visita [i]){
    goles.anotados <- goles.local[i] + goles.visita[i]
    
    if (goles.mayor.empate < goles.anotados)
    goles.mayor.empate <- goles.anotados
  }
}
paste("Goles del partido con mayor empate: ", goles.mayor.empate)


#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?
partidos.sin.goles <- 0

for(i in 1:length(goles.local)){
  if(goles.local[i] == 0 & goles.visita[i] == 0){
    partidos.sin.goles <- partidos.sin.goles + 1
  }
}
paste("Cantidad de partidos empatados a 0: ", partidos.sin.goles)

#RESULTADO OBTENIDO CON FUNCION TABLE COMO ORIGEN
paste("Cantidad de partidos empatados a 0: ", matriz.goles["0","0"])

#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
partidos.local.goleada.cero <- 0

for(i in 1:length(goles.local)){
  if(goles.local[i] >= 3 && goles.visita[i] == 0){
    partidos.local.goleada.cero <- partidos.local.goleada.cero + 1
  }
}
paste("Cantidad de partidos el local goleo a 0: ", partidos.local.goleada.cero)

#RESULTADO OBTENIDO CON FUNCION TABLE COMO ORIGEN
local.goleador <- sum(matriz.goles[4:length(matriz.goles[,"0"]),"0"])
paste("Cantidad de partidos el local goleo a 0: ", local.goleador)
