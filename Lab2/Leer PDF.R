# Universidad del Valle de Guatemala
# Data Science 1 - Seccion 10
# Maria Fernanda Estrada
# Christopher Sandoval
# Luis Delgado
# Estuardo Diaz



# Leer un pdf
install.packages("tabulizer")
library(tabulizer)
library(dplyr)
library(stringr)
pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

# Limpieza de datos
# Tomar unicamente las columnas de Anio, Mes, GasSuperior, GasRegular, Diesel y DieselLS (esta para completar los datos de Diesel)
datosImp <- datosImp[-c(46,96,146,196),]
datosImp <- subset(datosImp, select = c(Anio, Mes, GasSuperior, GasRegular, Diesel, DieselLS))
# Colocar NA en lugar de espacio o -
datosImp$Diesel[datosImp$Diesel=="-"] <- NA
datosImp$DieselLS[datosImp$DieselLS=="-"] <- NA
# Completar datos de Diesel con los de DieselLS y eliminar la columna restante
datosImp$Diesel[is.na(datosImp$Diesel)] <- datosImp$DieselLS[is.na(datosImp$Diesel)]
datosImp <- subset(datosImp, select = -c(DieselLS))
# Quitar , para que no haya problema con los numeros
datosImp$Diesel <- gsub(",", "", datosImp$Diesel)
datosImp$GasSuperior <- gsub(",", "", datosImp$GasSuperior)
datosImp$GasRegular <- gsub(",", "", datosImp$GasRegular)
# Decir cuales variables son cualitativas y numericas
datosImp$Anio <- as.factor(gsub(",", "", datosImp$Anio))
datosImp$Mes <- as.factor(gsub(",", "", datosImp$Mes))
datosImp$Diesel <- as.numeric(gsub(",", "", datosImp$Diesel))
datosImp$GasSuperior <- as.numeric(gsub(",", "", datosImp$GasSuperior))
datosImp$GasRegular <- as.numeric(gsub(",", "", datosImp$GasRegular))


# --------------- Analisis Exploratorio ---------------
# colocar codigo de luis


# --------------- Analisis Series de tiempo ---------------

# Paquetes y librerias
install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")
install.packages("tseries")
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

# Variable GasSuperior
ts_gas_superior <- ts(datosImp$GasSuperio, start=c(2001, 1), end=c(2020, 3), frequency=12) # Crear serie de tiempo
start(ts_gas_superior) # Inicio de serie
end(ts_gas_superior) # Fin de serie
frequency(ts_gas_superior) # Frecuencia de serie
#Ver el grafico de la serie
plot(ts_gas_superior)
abline(reg=lm(ts_gas_superior~time(ts_gas_superior)), col=c("red"))
# Descomposicion en componentes
dec.ts_gas_superior<-decompose(ts_gas_superior)
plot(dec.ts_gas_superior)
# Para saber si hay estacionariedad
adfTest(ts_gas_superior)
# Grafico de autocorrelacion
acf(ts_gas_superior)
# Funciones de autocorrelacion y autocorrelacion parcial
acf(diff(ts_gas_superior),12)
pacf(diff(ts_gas_superior))

# Variable GasRegular
ts_gas_regular <- ts(datosImp$GasRegular, start=c(2001, 1), end=c(2020, 3), frequency=12) # Crear serie de tiempo
start(ts_gas_regular) # Inicio de serie
end(ts_gas_regular) # Fin de serie
frequency(ts_gas_regular) # Frecuencia de serie
#Ver el grafico de la serie
plot(ts_gas_regular)
abline(reg=lm(ts_gas_regular~time(ts_gas_regular)), col=c("red"))
# Descomposicion en componentes
dec.ts_gas_regular<-decompose(ts_gas_regular)
plot(dec.ts_gas_regular)
# Para saber si hay estacionariedad
adfTest(ts_gas_regular)
# Grafico de autocorrelacion
acf(ts_gas_regular)
# Funciones de autocorrelacion y autocorrelacion parcial
acf(diff(ts_gas_regular),12)
pacf(diff(ts_gas_regular))


# Variable Diesel
ts_gas_diesel <- ts(datosImp$Diesel, start=c(2001, 1), end=c(2020, 3), frequency=12) # Crear serie de tiempo
start(ts_gas_diesel) # Inicio de serie
end(ts_gas_diesel) # Fin de serie
frequency(ts_gas_diesel) # Frecuencia de serie
#Ver el grafico de la serie
plot(ts_gas_diesel)
abline(reg=lm(ts_gas_diesel~time(ts_gas_diesel)), col=c("red"))
# Descomposicion en componentes
dec.ts_gas_diesel<-decompose(ts_gas_diesel)
plot(dec.ts_gas_diesel)
# Para saber si hay estacionariedad
adfTest(ts_gas_diesel)
# Grafico de autocorrelacion
acf(ts_gas_diesel)
# Funciones de autocorrelacion y autocorrelacion parcial
acf(diff(ts_gas_diesel),12)
pacf(diff(ts_gas_diesel))


# --------------- Generacion del modelo ---------------
# ***********pruebas **********
# Hacer el modelo
auto.arima(ts_gas_superior)

fit <- arima(log(ts_gas_superior), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(ts_gas_superior,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(ts_gas_superior), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)


# --------------- Prediccion con modelos generados ---------------
# ************pruebas***********

