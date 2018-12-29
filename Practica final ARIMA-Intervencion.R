
# Carga del dataset y primeros retoques
library(readxl)

data <- data.frame(read_xlsx("CuotaMercado.xlsx")) # Lectura del archivo
as.Date(data$Fecha, "%d/%m/%Y") -> data$Fecha # Transformación a formato fecha
str(data) # Ya se tienen las variables en su formato correcto

# Pasar los datos a formato serie temporal (xts y zoo)
library(zoo)
library(xts)

data.crest.xts <- to.weekly(xts(data$Crest, order.by = data$Fecha, frequency = 52))
data.crest.zoo <- as.zoo(data.crest.xts$`xts(data$Crest, order.by = data$Fecha, frequency = 52).Open`)
names(data.crest.zoo) <- "Cuota Crest"

data.colgate.xts <- to.weekly(xts(data$Colgate, order.by = data$Fecha, frequency = 52))
data.colgate.zoo <- as.zoo(data.colgate.xts$`xts(data$Colgate, order.by = data$Fecha, frequency = 52).Open`)
names(data.colgate.zoo) <- "Cuota Colgate"

# Se coge el Open arbitrariamente (son todas iguales) y se cambian los nombres por estética.

# Análisis exploratorio

# Primera aproximación gráfica para detectar estacionalidad (principalmente) en la evolución
library(ggplot2)
library(ggfortify)

autoplot(data.crest.zoo) + ggtitle("Cuota mercado Crest") + xlab("Años") + ylab("Cuota")

autoplot(data.colgate.zoo) + ggtitle("Cuota mercado Colgate") + xlab("Años") + ylab("Cuota")

# En ambos gráficos se puede observar el aumento de la cuota de Crest y disminución del Colgate
# pero no se observa una clara presencia de estacionalidad, sino las subidas/bajadas propias de unas ventas

hist(data$Crest, col = "red", xlab = "Cuota de mercado", ylab = "", 
     main = "Distribución de la cuota de mercado de Crest") # Presencia de binormal?
hist(data$Colgate, col = "blue", xlab = "Cuota de mercado", ylab = "",
     main = "Distribución de la cuota de mercado de Colgate") # Sigue una distribución normal (CLARÍSIMA)
cor(data$Crest, data$Colgate) # Gran relación (negativa) entre ambas variables 

# Creación muestras
periodos.omitidos <- 16 # Correspondientes al año 1963 (últimas semanas)
observ.muestra.crest <- length(data.crest.zoo)
muestra.pred.crest <- window(data.crest.zoo, start = index(data.crest.zoo[1]), 
                             end = index(data.crest.zoo[observ.muestra.crest - periodos.omitidos]))

observ.muestra.colgate <- length(data.colgate.zoo)
muestra.pred.colgate <- window(data.colgate.zoo, start = index(data.colgate.zoo[1]), 
                               end = index(data.colgate.zoo[observ.muestra.colgate - periodos.omitidos]))

# Modelo ARIMA

library(forecast)

crest.log <- log(data.crest.zoo) # Se pasa la serie a estacionaria en varianza
df.crest.log <- data.frame(Fecha = data$Fecha, Cuota = as.vector(crest.log)) # Dataframe con la serie ya estacionaria
ggplot(data = df.crest.log, mapping = aes(x = fecha, y = cuota)) + geom_line() +
  ggtitle("Crest estacionaria") # Gráfico de la serie

ggtsdisplay(diff(crest.log)) # Parece que una única diferencia será necesaria para tener una media estacionaria

colgate.log <- log(data.colgate.zoo) # Se pasa la serie a estacionaria en varianza
df.colgate.log <- data.frame(Fecha = data$Fecha, Cuota = as.vector(colgate.log))
ggplot(data = df.colgate.log, mapping = aes(x = fecha, y = cuota)) + geom_line() +
  ggtitle("Colgate estacionaria")
ggtsdisplay(diff(colgate.log))
