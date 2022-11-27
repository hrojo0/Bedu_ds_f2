library(DescTools)
library(dplyr)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
View(df)

total_intl_charge <- df$total_intl_charge

mean <- mean(total_intl_charge) #2.764582
sd <- sd(total_intl_charge) #0.7537726


"1. Grafica la distribución teórica de la variable aleatoria total_intl_charge"
(hist.cargos <-
    ggplot(df, aes(x = total_intl_charge)) +
    geom_histogram(aes(y = after_stat(density)),color = "#086788", fill = "#07A0C3", position = "identity", bins = 11, alpha = 0.4) +
    geom_vline(aes(xintercept = mean(total_intl_charge)), color = "#D16014", linetype = "dashed", size = .5) + 
    labs(title = "Cargos internacionales",x = "Mediciones", y = "Densidad") + 
    theme_light()
)
ggsave("Histograma cargos intl.jpg", plot = hist.cargos)

(curve.cargos <-
    ggplot(df, aes(x = total_intl_charge)) +
    xlim(0,5.5) + theme_light() +
    labs(title = "Cargos internacionales", x = "X", y = "f(x)") +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), color = "#086788", alpha = 0.7, linewidth = 0.9)
)
ggsave("Curva cargos intl.jpg", plot = curve.cargos)

"2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?" #11.25%
pnorm(1.85, mean, sd)


"3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?" #37.73%
pnorm(3, mean, sd, lower.tail =  FALSE)


"4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?" #70.6%
pnorm(4.85, mean, sd) - pnorm(2.35, mean, sd)


"5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?" #2.7 cargos
qnorm(0.48, mean, sd)


"6. ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?"
#1.798583 a 3.73058 cargos
qnorm(0.1, mean, sd); qnorm(0.1, mean, sd, FALSE) 
