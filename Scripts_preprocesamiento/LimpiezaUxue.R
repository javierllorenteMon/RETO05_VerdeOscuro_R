### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### PREPROCESAMIENTO DE DATOS

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)

# Cargar ficheros
pib_ipc_Paises <- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
exogenas <- read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

# Filtrar los datos por nuestro país (Italia)
pib_ipc_ITA <- pib_ipc_Paises %>% filter(Code == "ITA")
exogenas_ITA <- exogenas %>% filter(Code == "ITA")

# Analizar NAs
vis_miss(pib_ipc_ITA)
vis_miss(exogenas_ITA)
miss_var_summary(pib_ipc_ITA)
miss_var_summary(exogenas_ITA)

# Rellenar datos faltantes (son pocos asi que se buscan en internet y se imputan a mano)
pib_ipc_ITA$GDP.billion.currency.units[pib_ipc_ITA$Year == 2022 & pib_ipc_ITA$Month == 9] <- 500.653

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

str(pib_ITA)
str(ipc_ITA)
str(MS_ITA) # esta en character, convertir a numeric para que no de error
str(UR_ITA)
str(SMI_ITA) # esta en character, convertir a numeric para que no de error

MS_ITA$MS_t <- as.numeric(MS_ITA$MS_t)
SMI_ITA$SMI_t <- as.numeric(SMI_ITA$SMI_t)

str(MS_ITA)
str(SMI_ITA)

# Convertir a series temporales trimestrales
PIB_TS <- ts(pib_ITA$PIB_t, start = c(min(pib_ITA$Year), min(pib_ITA$Quarter)), end = c(2022, 3), frequency = 4)
IPC_TS <- ts(ipc_ITA$IPC_t, start = c(min(ipc_ITA$Year), min(ipc_ITA$Quarter)), end = c(2022, 3), frequency = 4)
MS_TS <- ts(MS_ITA$MS_t, start = c(min(MS_ITA$Year), min(MS_ITA$Quarter)), end = c(2022, 3), frequency = 4)
UR_TS <- ts(UR_ITA$UR_t, start = c(min(UR_ITA$Year), min(UR_ITA$Quarter)), end = c(2022, 3), frequency = 4)
SMI_TS <- ts(SMI_ITA$SMI_t, start = c(min(SMI_ITA$Year), min(SMI_ITA$Quarter)), end = c(2022, 3), frequency = 4)

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

# Detección y corrección de outliers
outliers_PIB <- tsoutliers(PIB_TS)
outliers_PIB # index = posición del outlier, replacements = valor corregido sugerido
PIB_sinO <- tsclean(PIB_TS) # reemplaza los outliers, y rellena NAs si los hay

outliers_IPC <- tsoutliers(IPC_TS)
outliers_IPC
IPC_sinO <- tsclean(IPC_TS)

outliers_MS <- tsoutliers(MS_TS)
outliers_MS
MS_sinO <- tsclean(MS_TS)

outliers_UR <- tsoutliers(UR_TS)
outliers_UR
UR_sinO <- tsclean(UR_TS)

outliers_SMI <- tsoutliers(SMI_TS)
outliers_SMI 
SMI_sinO <- tsclean(SMI_TS)

# Graficar para comparar las series temporales con y sin outliers

#PIB
df_PIB <- data.frame(
  Year = rep(1996:2022, each=4)[1:length(PIB_TS)],
  Quarter = rep(1:4, times=length(1996:2022))[1:length(PIB_TS)],
  PIB = as.numeric(PIB_TS),
  PIB_sinO = as.numeric(PIB_sinO)
)

grafico_outliers_PIB <- ggplot(df_PIB, aes(x = Year + (Quarter-1)/4)) +
  geom_line(aes(y = PIB, color="Original"), size = 0.8) +
  geom_line(aes(y = PIB_sinO, color="Sin Outliers"), size = 1) +
  scale_color_manual(values = c("Original" = "gray", "Sin Outliers" = "red")) +
  labs(x = "Año", y = "PIB (miles de millones €)", color = "Serie")

# IPC
df_IPC <- data.frame(
  Year = rep(1996:2022, each = 4)[1:length(IPC_TS)],
  Quarter = rep(1:4, times = length(1996:2022))[1:length(IPC_TS)],
  IPC = as.numeric(IPC_TS),
  IPC_sinO = as.numeric(IPC_sinO)
)

grafico_outliers_IPC <- ggplot(df_IPC, aes(x=Year + (Quarter-1)/4)) +
  geom_line(aes(y = IPC, color = "Original"), size = 0.8) +
  geom_line(aes(y = IPC_sinO, color = "Sin Outliers"), size = 1) +
  scale_color_manual(values = c("Original" = "gray", "Sin Outliers" = "red")) +
  labs(x = "Año", y = "IPC", color = "Serie")

#Money SUpply
df_MS <- data.frame(
  Year = rep(1996:2022, each = 4)[1:length(MS_TS)],
  Quarter = rep(1:4, times = length(1996:2022))[1:length(MS_TS)],
  MS = as.numeric(MS_TS),
  MS_sinO = as.numeric(MS_sinO)
)

grafico_outliers_MS <- ggplot(df_MS, aes(x = Year + (Quarter-1)/4)) +
  geom_line(aes(y = MS, color = "Original"), size = 0.8) +
  geom_line(aes(y = MS_sinO, color="Sin Outliers"), size = 1) +
  scale_color_manual(values = c("Original" = "gray", "Sin Outliers" = "red")) +
  labs(x = "Año", y = "Money Supply", color = "Serie")

# Unemployment Rate 
df_UR <- data.frame(
  Year = rep(1996:2022, each = 4)[1:length(UR_TS)],
  Quarter = rep(1:4, times = length(1996:2022))[1:length(UR_TS)],
  UR = as.numeric(UR_TS),
  UR_sinO = as.numeric(UR_sinO)
)

grafico_outliers_UR <- ggplot(df_UR, aes(x = Year + (Quarter-1)/4)) +
  geom_line(aes(y = UR, color = "Original"), size = 0.8) +
  geom_line(aes(y = UR_sinO, color = "Sin Outliers"), size = 1) +
  scale_color_manual(values = c("Original" = "gray", "Sin Outliers" = "red")) +
  labs(x = "Año", y = "Tasa de desempleo (%)", color = "Serie")

# Stock Market Index
df_SMI <- data.frame(
  Year = rep(1996:2022, each = 4)[1:length(SMI_TS)],
  Quarter = rep(1:4, times = length(1996:2022))[1:length(SMI_TS)],
  SMI = as.numeric(SMI_TS),
  SMI_sinO = as.numeric(SMI_sinO)
)

grafico_outliers_SMI <- ggplot(df_SMI, aes(x = Year + (Quarter-1)/4)) +
  geom_line(aes(y = SMI, color = "Original"), size = 0.8) +
  geom_line(aes(y = SMI_sinO, color = "Sin Outliers"), size = 1) +
  scale_color_manual(values = c("Original" = "gray", "Sin Outliers" = "red")) +
  labs(x = "Año", y = "Stock Market Index", color = "Serie")

# Guardar los datos limpios
saveRDS(PIB_sinO, "Datos/transformados/PIB_sinO.rds")
saveRDS(IPC_sinO, "Datos/transformados/IPC_sinO.rds")
saveRDS(MS_sinO, "Datos/transformados/MS_sinO.rds")
saveRDS(UR_sinO, "Datos/transformados/UR_sinO.rds")
saveRDS(SMI_sinO, "Datos/transformados/SMI_sinO.rds")

# Guardar graficos
ggsave(grafico_outliers_PIB, "Graficos/Graficos prerprocesamiento/GraficoOutliersPIB.pdf", )
ggsave(grafico_outliers_IPC, "Graficos/Graficos prerprocesamiento/GraficoOutliersIPC.pdf")
ggsave(grafico_outliers_MS, "Graficos/Graficos prerprocesamiento/GraficoOutliersMS.pdf")
ggsave(grafico_outliers_UR, "Graficos/Graficos prerprocesamiento/GraficoOutliersUR.pdf")
ggsave(grafico_outliers_SMI, "Graficos/Graficos prerprocesamiento/GraficoOutliersSMI.pdf")
