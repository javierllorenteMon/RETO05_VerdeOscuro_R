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

# Eliminar filas con NA para dejar los datos trimestrales (el PIB ya esta trimestral, por eso con eliminar las filas vacias vale)
pib_ITA <- pib_ipc_ITA %>% 
  filter(!is.na(GDP.billion.currency.units)) %>% 
  mutate(Quarter = Month/3) %>%
  select(Year, Quarter, PIB_t = GDP.billion.currency.units)

# Pasar todos lo demás datos a trimestrales (estan mensuales)
ipc_ITA <- pib_ipc_ITA %>% 
  group_by(Year, Quarter = ceiling(Month/3)) %>%
  summarise(IPC_t = mean(Consumer.Price.Index..CPI., na.rm = TRUE)) %>%
  ungroup()

MS_ITA <- exogenas_ITA %>%
  filter(Month %in% c(3,6,9,12)) %>%
  mutate(Quarter = Month/3) %>%
  select(Year, Quarter, MS_t = Money.supply.billion.currency.units)

UR_ITA <- exogenas_ITA %>%
  group_by(Year, Quarter = ceiling(Month/3)) %>%
  summarise(UR_t = mean(as.numeric(Unemployment.rate.percent), na.rm = TRUE)) %>%
  ungroup()

SMI_ITA <- exogenas_ITA %>%
  filter(Month %in% c(3,6,9,12)) %>%
  mutate(Quarter = Month/3) %>%
  select(Year, Quarter, SMI_t = Stock.market.index)


# Convertir a series temporales trimestrales
PIB_TS <- ts(pib_ITA$PIB_t, start = c(min(pib_ITA$Year), min(pib_ITA$Quarter)), end = c(2022, 3), frequency = 4)
IPC_TS <- ts(ipc_ITA$IPC_t, start = c(min(ipc_ITA$Year), min(ipc_ITA$Quarter)), end = c(2022, 3), frequency = 4)
MS_TS <- ts(MS_ITA$MS_t, start = c(min(MS_ITA$Year), min(MS_ITA$Quarter)), end = c(2022, 3), frequency = 4)
UR_TS <- ts(UR_ITA$UR_t, start = c(min(UR_ITA$Year), min(UR_ITA$Quarter)), end = c(2022, 3), frequency = 4)
SMI_TS <- ts(SMI_ITA$SMI_q, start = c(min(SMI_ITA$Year), min(SMI_ITA$Quarter)), end = c(2022, 3), frequency = 4)

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

decomIPC <- decompose(IPC_TS)
autoplot(decomIPC)

# 

