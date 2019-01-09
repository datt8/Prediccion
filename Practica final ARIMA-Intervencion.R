
# Carga del dataset y primeros retoques
library(readxl)

data <- data.frame(read_xlsx("CuotaMercado.xlsx")) # Lectura del archivo
as.Date(data$Fecha, "%Y/%m/%d") -> data$Fecha # Transformación a formato fecha
str(data) # Ya se tienen las variables en su formato correcto

# Pasar los datos a formato serie temporal (xts y zoo)
library(zoo)
library(xts)

data.crest.xts <- xts(data$Crest, order.by = data$Fecha)
data.crest.zoo <- as.zoo(data.crest.xts)
names(data.crest.zoo) <- "Cuota Crest"

data.colgate.xts <- xts(data$Colgate, order.by = data$Fecha)
data.colgate.zoo <- as.zoo(data.colgate.xts)
names(data.colgate.zoo) <- "Cuota Colgate"

# Se cambian los nombres por estética.

# Análisis exploratorio

# Primera aproximación gráfica para detectar estacionalidad (principalmente) en la evolución
library(ggplot2)
library(ggfortify)

autoplot(data.crest.zoo) + ggtitle("Cuota mercado Crest") + xlab("Años") + ylab("Cuota")

autoplot(data.colgate.zoo) + ggtitle("Cuota mercado Colgate") + xlab("Años") + ylab("Cuota")

# En ambos gráficos se puede observar el aumento de la cuota de Crest y disminución del Colgate
# pero no se observa una clara presencia de estacionalidad, sino las subidas/bajadas propias de unas ventas

hist(data$Crest, col = "red", xlab = "Cuota de mercado", ylab = "", 
     main = "Distribución de la cuota de mercado de Crest") # Presencia de binormal
hist(data$Colgate, col = "blue", xlab = "Cuota de mercado", ylab = "",
     main = "Distribución de la cuota de mercado de Colgate") # Sigue una distribución normal (CLARÍSIMA)
cor(data$Crest, data$Colgate) # Gran relación (negativa) entre ambas variables 

# Creación muestras

periodos.omitidos <- 16 # Correspondientes al año 1963 (últimas semanas)
observ.muestra.crest <- length(data.crest.zoo)
muestra.pred.crest <- window(data.crest.zoo, start = index(data.crest.zoo[1]), 
                             end = index(data.crest.zoo[observ.muestra.crest - periodos.omitidos]))

# Se crean ambas muestras para la posterior realización del ARIMA

observ.muestra.colgate <- length(data.colgate.zoo)
muestra.pred.colgate <- window(data.colgate.zoo, start = index(data.colgate.zoo[1]), 
                               end = index(data.colgate.zoo[observ.muestra.colgate - periodos.omitidos]))

# Modelo ARIMA

library(forecast)

crest.log <- log(data.crest.zoo) # Se pasa la serie a estacionaria en varianza
ggtsdisplay(diff(crest.log)) # Parece que una única diferencia será necesaria para tener una media estacionaria

colgate.log <- log(data.colgate.zoo) # Se pasa la serie a estacionaria en varianza
ggtsdisplay(diff(colgate.log)) # Parece que como en el caso anterior, será necesaria una única diferencia para la estacionariedad

# Ambas series parece que poseen una correlación parcial a 3-4 semanas (dando pistas sobre una correlación parcial mensual)

## Predicción del ARIMA Crest

arima.modelo.crest <- auto.arima(muestra.pred.crest, lambda = 0)
summary(arima.modelo.crest) # El modelo resultante es una ARIMA(0,1,1) (sin componente estacional)
ggtsdisplay(arima.modelo.crest$residuals) # Si está entre las líneas azules (los residuos son irrelevantes -> ruido blanco)
Box.test(arima.modelo.crest$residuals, lag = 3, fitdf = 1, type = "Lj") # Se confirma el ruido blanco. Las referencias muestran que el fitdf óptimo es p+q
modelo.arima.prediccion.crest <- forecast(arima.modelo.crest, h = 16)

## Predicción del ARIMA Colgate

arima.modelo.colgate <- auto.arima(muestra.pred.colgate, lambda = 0)
summary(arima.modelo.colgate) # El modelo resultante es una ARIMA(0,1,1) (sin componente estacional)
ggtsdisplay(arima.modelo.colgate$residuals) # Los residuos son irrelevantes -> ruido blanco
Box.test(arima.modelo.colgate$residuals, lag = 4, fitdf = 1, type = "Lj") # Se confirma el ruido blanco
modelo.arima.prediccion.colgate <- forecast(arima.modelo.colgate)

## Comparación de la realidad vs. Crest y Colgate

arima.crest <- modelo.arima.prediccion.crest$mean # Esta es la predicción que da el modelo
real.crest <- data.crest.zoo[(observ.muestra.crest - periodos.omitidos + 1):observ.muestra.crest] # La observación real
comparacion.crest <- data.frame(matrix(c(arima.crest, real.crest))) # Se une en una data frame
names(comparacion.crest) <- c("ARIMA", "Real") # Se cambian los nombres de las columnas

arima.colgate <- modelo.arima.prediccion.colgate$mean # Predicción del modelo
real.colgate <- data.colgate.zoo[(observ.muestra.colgate - periodos.omitidos + 1):observ.muestra.colgate] # Observación real de la marca
comparacion.colgate <- data.frame(matrix(c(arima.colgate, real.colgate))) # Dataframe con la observación predicha y la real
names(comparacion.colgate) <- c("ARIMA", "Real") # Se cambian los nombres

## Gráficos de ambas para ver la predicción

# Detección atípicos

library(TSA)
detectAO(arima.modelo.crest) # Atípico en semana 136 (1 después a decisión ADA)
detectAO(arima.modelo.colgate) # No hay

detectIO(arima.modelo.crest) # Atípico en semana 99
detectIO(arima.modelo.colgate) # Atípico en semana 102
# Puede ser que ambos estén afectados por expropiaciones cubanas

## Corrección atípicos

arima.modelo.crest2 <- arimax(muestra.pred.crest, order = c(0,1,1), io = c(99)) # Corrige el innovativo de Crest
detectIO(arima.modelo.crest2) # Ya no hay atípico innovativo