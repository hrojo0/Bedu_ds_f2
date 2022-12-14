######################## SESIÓN 7: SERIES DE TIEMPO  ##########################
###############################################################################

## EJEMPLO 01: MANIPULACIÓN DE SERIES DE TIEMPO
"Una serie de tiempo representa una secuencia de datos ordenados de forma cronológica
y secuencial, por lo que para su análisis es necesario que todas las observaciones 
estén registradas con un índice en tiempo discreto y que las observaciones estén en
intervalos de tiempo equidistantes.

Veamos cómo analizar y manipular series de tiempo en R. Para ello vamos a comenzar 
analizando el siguiente dataframe:"
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/cbe_data.csv"
CBE <- read.csv(url, header = TRUE)
head(CBE)
class(CBE)

"La función ts() nos permite crear un objeto de serie de tiempo a partir de un vector.
Basta con establecer los siguientes argumentos
- start: Periodo inicial de la serie
- end: Periodo inicial de la seria
- freq: Número de observaciones por unidad de tiempo

Vamos a crear 3 series de tiempo mensuales para los datos que tenemos:
frecuencia:
1 anuales
4 trismestrales
12 mensuales
52 semanales"
Elec.ts <- ts(CBE[,3], start = c(1958,1), frequency = 12)
Beer.ts <- ts(CBE[,2], start = c(1958,1), frequency = 12)
Choc.ts <- ts(CBE[,1], start = c(1958,1), frequency = 12)
class(Elec.ts);class(Beer.ts);class(Choc.ts)

plot(cbind(Elec.ts, Beer.ts, Choc.ts), 
     main = "Producción de Chocolate, Cerveza y Electricidad", 
     xlab = "Tiempo",
     sub = "Enero de 1958 - Diciembre de 1990")

## EJEMPLO 03: CAMINATA ALEATORIA Y FUNCIONES DE AUTOCORRELACIÓN
"Para poder realizar estimaciones y predicciones de series de tiempo, los modelos 
clásicos requieren que las series de tiempo sean estacionarias, es decir, que su 
promedio (valor de tendencia de largo plazo) sea constante al igual que varianza.

estacionariedad = oscila alrededor de su promedio regresa a largo plaza, varianza constante a través del tiempo

Para entender cómo se comporta una serie de tiempo, vamos a analizar el modelo de 
ruido blanco ~ N(0,1)"
set.seed(3)
w <- rnorm(300)
plot(w, type = "l", xlab = "")
title(main = "Ruido Blanco Gaussiano", xlab = "Tiempo")

mean(w);sd(w)

"La función de AUTOCORRELACION es una medida de la correlación entre las observaciones 
temporales separadas por k rezagos."
acf(w)

"La función de AUTOCORRELACIÓN PARCIAL es una medida de la correlación entre las observaciones 
temporales separadas por k rezagos, tomando en cuenta los valores de los intervalos intermedios"
pacf(w)

"En una serie de ruído blanco, la AC y la ACP no tiene valores significativos en los 
rezagos de la variable."

"Un ejemplo de serie no estacionaria es el modelo de caminada aleatoria. En este modelo
tanto el promedio como la varianza dependen fuertemente del tiempo y sus incrementos son
ruído blanco:"
x <- w <- rnorm(1000)
for(t in 2:1000) x[t] <- x[t-1] + w[t]
  


plot(x, type = "l", main = "Caminata Aleatoria Simulada", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))

"Este es el comportamiento de la gran mayoría de las series de tiempo, lo que hace 
muy complicado utilizarlas directamente para hacer estimaciones o predicción.
Veamos como se comportan sus funciones de AC y ACP:"
acf(x)
pacf(x)

"Como se observa, la función de ACP no muestra rezagos significativos, sin embargo, 
la función de AC muestra una serie de tiempo totalmente correlacionada. Para nuestra 
suerte, muchas de las variables económicas y financieras es posible convertirlas en 
estacionarias calculando la primera diferencia; es decir, el cambio entre un periodo 
en el tiempo y otro:"

plot(diff(x), type = "l", main = "Primera diferencia de X", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))

acf(diff(x))
pacf(diff(x))

## EJEMPLO 04: MODELOS ARIMA
"ARIMA representa modelos Autorregresivos (AR) Integrados (I) de Media Móvil (MA) y se 
representan como ARIMA(q,d,p):
- q: Número de términos autorregresivos (de la variable dependiente)
- d: Número de diferencias aplicadas a la variable para hacerla estacionaria
- p: Número de rezagos en el término de error

Veamos un ejemplo de serie de tiempo para determinar al modelo ARIMA a estimar:"
set.seed(3)
x <- w <- rnorm(1000)
for(i in 3:1000) x[i] <- 0.5*x[i-1] + x[i-1] - 0.5*x[i-2] + w[i] + 0.3*w[i-1]

plot(x, type = "l", 
     main = "Serie simulada de un modelo ARIMA(1, 1, 1)",
     xlab = "Tiempo",
     ylab = expression(x[t]),
     sub = expression(x[t] == 0.5*x[t-1] + x[t-1] - 0.5*x[t-2] + w[t] + 0.3*w[t-1]))

acf(x)
pacf(x)

"Como podemos ver, la serie de tiempo es no estacionaria. Calculemos la primera diferencia:"
acf(diff(x))
pacf(diff(x))

"Esta transformación nos ayuda a tener una mejor idea del número de términos autorregresivos 
y de medioa móvil que incluir en el modelo. Como era de esperarse, el modelo a estimar 
debe tener I(1) y MA(1). Sin embargo, vemos que la función de AC muestra desde 1 a 3 
términos autorregresivos. Para determinar el mejor modelo, vamos a estimar los modelos 
propuestosy analizar los criterios de información de Akaike (AIC):"
arima(x, order = c(1, 1, 1)) #x[t] = 0.4582*x[t-1]+0.3459*w[t-1]+w[t]
arima(x, order = c(2, 1, 1)) #x[t] = 0.5286*x[t-1]-0.0542*x[t-1]+0.2791*w[t-1]+w[t]
arima(x, order = c(3, 1, 1))
#AIC: Akaike Information Criterion


"El mejor modelo es aquel que tenga menor AIC.
Ahora pongamos esto en práctica para la producción de electricidad:"
plot(Elec.ts, xlab = "", ylab = "")
title(main = "Serie de Producción de Electricidad Australiana",
      ylab = "Producción de electricidad (GWh)",
      xlab = "Tiempo")

plot(diff(Elec.ts), xlab = "", ylab = "")
title(main = "Serie Diferenciada de Producción de Electricidad Australiana",
      xlab = "Tiempo", ylab = "Dif Serie",
      sub = "Gráfica de la serie diferenciada de primer Órden")

plot(diff(log(Elec.ts)), xlab = "", ylab = "")
title(main = "Serie de log dif de Producción de Electricidad Australiana",
      xlab = "Tiempo", ylab = "Dif log-Serie",
      sub = "Gráfica de la serie log-transformada diferenciada de primer órden")

acf(diff(log(Elec.ts)))
pacf(diff(log(Elec.ts)))


fit <- arima(log(Elec.ts), order = c(0, 1, 1), seas = c(2, 0, 2)) #seas = efecto estacional
fit

(pr <- predict(fit, 12)$pred)
ts.plot(cbind(window(Elec.ts, start = 1981), exp(pr)), col = c("blue", "red"), xlab = "")
title(main = "Predicción para la serie de producción de electricidad",
      xlab = "Mes",
      ylab = "Producción de electricidad (GWh)")