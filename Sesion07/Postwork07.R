"Utilizando el siguiente vector numérico, realiza lo que se indica:"
  
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
class(Global)

"1.Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856"
Globa.ts <- ts(Global, start = c(1856,1), frequency = 12)
class(Globa.ts)


"2.Realiza una gráfica de la serie de tiempo anteriorde 2005"
plot(Globa.ts, 
     main = "Temperatura global", 
     xlab = "Tiempo",
     sub = "Enero de 1958 - Diciembre de 2005")


"3.Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:"
plot(diff(Globa.ts), type = "l", main = "Primera diferencia de X", 
     xlab = "Tiempo", ylab = expression(Globa.ts[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))


"4.¿Consideras que la serie es estacionaria en niveles o en primera diferencia?"
#Es estacionaria en primera diferencia ya que se observa como se normaliza


"5.Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?"
acf(diff(Globa.ts))
pacf(diff(Globa.ts))