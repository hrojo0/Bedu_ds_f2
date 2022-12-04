library(ggplot2)
#(Fuel Economy Data)
View(mpg)
head(mpg)

"1. Con base en los datos, existe evidencia estadística para concluir que, en promedio,
los coches producidos entre 1999 y 2008 podían recorrer más de 22.8 millas de carretera por galón (hwy)?"
#Ho: mu <= 22.8
#Ha: mu > 22.8
t.test(x = mpg$hwy, alternative = "greater", mu = 22.8) #p-value = 0.05071
p.value <- 0.05071
if(p.value < 0.05) {
  paste("NC 95%: Existe evidencia para rechazar Ho, no podían recorrer 22.8 millas")
} else{
  paste("NC 95%: No existe evidencia para rechazar Ho, si podían recorrer 22.8 millas")
}

"2. Con base en los datos, existe evidencia estadística para concluir que, en promedio,
el desplazamiento del motor en litros (displ) para los coches producidos entre 1999 y 2008 era mayor o igual 3.7 litros?"
#Ho: mu >= 3.7
#Ha: mu < 3.7
t.test(x = mpg$displ, alternative = "less", mu = 3.7) #p-value = 0.0037
p.value <- 0.0037
if(p.value < 0.05) {
  paste("NC 95%: Existe evidencia para rechazar Ho, el desplazamiento era menor a 3.7 lt")
} else{
  paste("NC 95%: No existe evidencia para rechazar Ho, el desplazamiento era mayor a 3.7 lt ")
}

  
"3. Con base en los datos, existe evidencia estadística para concluir que, en promedio,
los motores con 4 cilindros (cyl = 4) tienen un mayor rendimiento en millas de carretera por galón (hwy)
que los motores con 6 cilindros (cyl = 6)"
#Ho: cyl_hwy_4 <= cyl_hwy_6
#Ha: cyl_hwy_4 > cyl_hwy_6
var.test(unlist(mpg[mpg$cyl == 4, "hwy"]), 
         unlist(mpg[mpg$cyl == 6, "hwy"]), 
         ratio = 1, alternative = "two.sided")

t.test(x = mpg[mpg$cyl == 4, "hwy"], 
       y = mpg[mpg$cyl == 6, "hwy"],
       alternative = "two.sided", mu = 0, var.equal = TRUE)

p.value <- 2.521e-16
if(p.value < 0.05) {
  paste("NC 95%: Existe evidencia para rechazar Ho, el rendimiento de 6 cilindros es mayor que 4 cilindros")
} else{
  paste("NC 95%: No existe evidencia para rechazar Ho, el rendimiento de 4 cilindros es mayor que 6 cilindros")
}
