ventas.a <- c(18,26,14,22,34,46,37)
ventas.b <- c(58,28,35,16,5,19,11)

devoluciones.a <- c(0,4,2,0,12,20,8)
devoluciones.b <- c(25,0,8,0,0,0,2)

#A) Calcula las unidades de ventas netas de cada día y para cada producto:
(ventas.netas.a <- ventas.a - devoluciones.a)
(ventas.netas.b <- ventas.b - devoluciones.b)

#B) Calcula los ingresos de cada día y para cada producto:
(ingresos.netos.a <- ventas.netas.a * 15.5)
(ingresos.netos.b <- ventas.netas.b * 7.8)

#C) Calculo los costos de cada día y para cada producto:
(costos.netos.a <- ventas.netas.a * 5.9)
(costos.netos.b <- ventas.netas.b * 2.4)

#D) Con los vectores de ingreso y costos para cada producto, crea una matriz de ingresos y otra de costos diarios
(ingresos.dia <- rbind(ingresos.netos.a, ingresos.netos.b))
(costos.dia <- rbind(costos.netos.a, costos.netos.b))

#E) Usando las matrices anteriores, calcula la utilidad bruta y neta por día
(utilidad.bruta.dia <- ingresos.dia - costos.dia)
(utilidad.neta.dia <- (1-0.1) * utilidad.bruta.dia)

#F) Considerando las funciones colSums() y rowSums, utiliza la adecuada para calular la utilidad semanal por producto
(utilidad.neta.prod <- rowSums(utilidad.neta.dia))

#G) Utilizando una función de R, sumo los elementos de tu resultado anterior para conocer la utilidad total de la semana
(utilidad.semanal <- sum(utilidad.neta.prod))
