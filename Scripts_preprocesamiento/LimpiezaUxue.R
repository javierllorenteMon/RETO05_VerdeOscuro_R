### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)

# Cargar ficheros
PIB_IPC_Paises <- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
exogenas <- read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

# Filtrar los datos por nuestro país (Italia)
pib_ipc_ITA <- PIB_IPC_Paises %>% filter(Code == "ITA")
exogenas_ITA <- exogenas %>% filter(Code == "ITA")

# Analizar NAs
vis_miss(pib_ipc_ITA)
vis_miss(exogenas_ITA)
miss_var_summary(pib_ipc_ITA)
miss_var_summary(exogenas_ITA)

# Eliminar filas con NA para dejar los datos trimestrales (el PIB ya estan trimestrales)
pib_ITA <- pib_ipc_ITA %>% filter(!is.na(GDP.billion.currency.units)) %>% select(GDP.billion.currency.units)

# Dejar los datos de "exogenas" trimestralmente (estan mensuales)
#exogenas_ITA <- exogenas_ITA %>% filter(Month %in% c(3,6,9,12))

# Convertir a series temporales trimestrales
PIB_TS <- ts(pib_ITA,start = c(1996,1), frequency = 4)
IPC_TS <- ts(pib_ipc_ITA$Consumer.Price.Index..CPI. ,start = c(1996,1), frequency = 12)
MS_TS <- ts(exogenas_ITA$Money.supply.billion.currency.units, start = c(1999, 1), frequency = 12)
UR_TS <- ts(exogenas_ITA$Unemployment.rate.percent, start = c(1996, 1), frequency = 12)
SMI_TS <- ts(exogenas_ITA$Stock.market.index, start = c(1996, 1), frequency = 12)

# Analizar series temporales
class(PIB_TS)
time(PIB_TS)
frequency(PIB_TS)
start(PIB_TS)
end(PIB_TS)
autoplot(PIB_TS)
autoplot(diff(PIB_TS))
acf(PIB_TS)

class(IPC_TS)
time(IPC_TS)
autoplot(IPC_TS)
frequency(IPC_TS)
start(IPC_TS)
end(IPC_TS)
autoplot(IPC_TS)
autoplot(diff(IPC_TS))
acf(IPC_TS)

class(MS_TS)
time(MS_TS)
autoplot(MS_TS)
frequency(MS_TS)
start(MS_TS)
end(MS_TS)
autoplot(MS_TS)
autoplot(diff(MS_TS))
acf(MS_TS)

class(UR_TS)
time(UR_TS)
autoplot(UR_TS)
frequency(UR_TS)
start(UR_TS)
end(UR_TS)
autoplot(UR_TS)
autoplot(diff(UR_TS))
acf(UR_TS)

class(SMI_TS)
time(SMI_TS)
autoplot(SMI_TS)
frequency(SMI_TS)
start(SMI_TS)
end(SMI_TS)
autoplot(SMI_TS)
autoplot(diff(SMI_TS))
acf(SMI_TS)

# Descomposicion de componentes
decomPIB <- decompose(PIB_TS)
autoplot(decomPIB)

decomPNC <- decompose(PNC_TS)
autoplot(decomPNC)

# 

