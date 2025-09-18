# Librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
# Ficheros
Pib_Paises <-  read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
Exogenas <-  read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')



# Limpieza ----
# Eliminar columnas que no sirven
datos1 <- Pib_Paises %>% filter(Code=="ITA")
datos2 <- Exogenas %>%  filter(Code=="ITA")
vis_miss(datos1,cluster = T)
vis_miss(datos2,cluster = T)
miss_var_summary(datos1)

# Eliminar filas con NA
Datos1 <- datos1 %>% filter(!is.na(GDP.billion.currency.units))
vis_miss(Datos1,cluster = T)



#Convertir a serie temporal
GDP_TS <- ts(Datos1$GDP.billion.currency.units ,start = c(1996,1), frequency = 4)
class(GDP_TS)
time(GDP_TS)
autoplot(GDP_TS)


CPI_TS <- ts(Datos1$Consumer.Price.Index..CPI. ,start = c(1996,1), frequency = 4)
class(CPI_TS)
time(CPI_TS)
autoplot(CPI_TS)


y_multi <- cbind(
  PIB = Datos1$GDP.billion.currency.units,
  CPI = Datos1$Consumer.Price.Index..CPI..
)


q_start <- Datos1$Month[1] / 3           # 3->1er trim, 6->2º, etc.
TS_MULTI <- ts(y_multi, start = c(Datos1$Year[1], q_start), frequency = 4)
class(TS_MULTI)
autoplot(TS_MULTI)


#write.csv(datos, file = file.path(path_datos_limpios,datos_limpios), row.names = FALSE)



