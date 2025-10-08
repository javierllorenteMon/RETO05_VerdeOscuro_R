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

# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")

# Descomposición de componentes
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)

# Train(2000,1)(2021,2) test(2021,2)(2022,2)
train_PIB <- window(PIB_sinO, start = c(2000, 1), end = c(2021, 1))
test_PIB <- window(PIB_sinO, start = c(2021, 2), end = c(2022, 2))

# Comprobar estacionariedad con los test (seguramente sea no estacionaria porque no las hemos hecho estacionaria todavía)

