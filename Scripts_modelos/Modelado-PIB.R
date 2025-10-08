### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL PIB

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)
source("Scripts_Preprocesamiento/Funciones.R")

# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")

# Descomposición de componentes
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)

# Train(2000,1)(2021,2) test(2021,2)(2022,2)
train_PIB <- window(PIB_sinO, start = c(2000, 1), end = c(2021, 1))
test_PIB <- window(PIB_sinO, start = c(2021, 2), end = c(2022, 2))

# Comprobar estacionariedad con los test (seguramente sea no estacionaria porque no la hemos hecho estacionaria todavía)
test_estacionariedad(train_PIB, nombre = "PIB")

# Comprobar si tiene varianza creciente para ver si aplicarle log
ts.plot(train_PIB) # si parece tener varianza creciente

# Ahora comprobar estacionalidad para ver si meterle lag en el diff()
# Con nsdiffs (que evalua si hace falta diferencia estacional) si da 1 tiene estacionalidad si da 0 no
nsdiffs(train_PIB)

# Tambien comprobamos la estacionalidad con acf (si tiene pico en lag = 4 hay estacionalidad) (si decae lentamente o no muestra patrón periódico, sin estacionalidad fuerte)
acf(train_PIB) # no tiene pico relevante en lag = 4

# Test de estacionariedad otra vez (esta vez ajustando la serie)
tsdisplay(train_PIB)
train_PIB_est <- diff(log(PIB_sinO), differences = 1)
tsdisplay(train_PIB_est)
test_estacionariedad(train_PIB_est, nombre = "PIB")
