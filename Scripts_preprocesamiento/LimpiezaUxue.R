### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)

# Cargar ficheros
PIB_IPC_Paises <- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
Exogenas <- read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

# Filtrar los datos por nuestro país (Italia)
pib_ipc_ITA <- PIB_IPC_Paises %>% filter(Code=="ITA")
exogenas_ITA <- Exogenas %>%  filter(Code=="ITA")

# Analizar NAs
vis_miss(pib_ipc_ITA)
vis_miss(exogenas_ITA)
miss_var_summary(pib_ipc_ITA)
miss_var_summary(exogenas_ITA)

# Eliminar filas con NA para dejar los datos trimestrales
pib_ipc_ITA<- pib_ipc_ITA %>% filter(!is.na(GDP.billion.currency.units))
vis_miss(pib_ipc_ITA)

# Convertir a serie temporal
PIB_TS <- ts(pib_ipc_ITA$GDP.billion.currency.units ,start = c(1996,1), frequency = 4)
PNC_TS <- ts(pib_ipc_ITA$Consumer.Price.Index..CPI. ,start = c(1996,1), frequency = 4)

# Analizar series temporales
class(PIB_TS)
time(PIB_TS)
autoplot(PIB_TS)
frequency(PIB_TS)
start(PIB_TS)
end(PIB_TS)

class(PNC_TS)
time(PNC_TS)
autoplot(PNC_TS)
frequency(PNC_TS)
start(PNC_TS)
end(PNC_TS)

# Descomposicion de componentes
decomPIB <- decompose(PIB_TS)
autoplot(decomPIB)

decomPNC <- decompose(PNC_TS)
autoplot(decomPNC)
