library(dplyr)
library(readr)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(zoo)

################################################################################
################################################################################
######### "Índice Trimestral de Empleo Asalariado - Italia (2000-2025)"#########
################################################################################
################################################################################
# Incluye TODA la industria + servicios de Italia (secciones B a N de la clasificación NACE)
# Es el sector más amplio y representativo de la economía italiana
#Excluye solo: Agricultura (A) y Administración Pública (O)

datos <- read_delim("Datos/Istat/cuanto trabajo hay (number of payrolls jobs).csv", delim = ";")

# Usar "TOTAL INDUSTRY AND SERVICES (b to n)" que tiene datos desde 2000
evolucion_empleo <- datos %>%
  select(TIME_PERIOD, Observation, `Economic activity (NACE Rev. 2)`) %>%
  rename(Periodo = TIME_PERIOD, Indice = Observation, Sector = `Economic activity (NACE Rev. 2)`) %>%
  filter(grepl("Q", Periodo)) %>%
  mutate(
    Año = as.numeric(substr(Periodo, 1, 4)),
    Trimestre = paste0("Q", as.numeric(substr(Periodo, 7, 7))),  # Agregar "Q" aquí
    Indice = as.numeric(Indice)
  ) %>%
  filter(Sector == "TOTAL INDUSTRY AND SERVICES (b to n)") %>%
  select(Año, Trimestre, Indice) %>%
  arrange(Año, Trimestre)


write_csv(evolucion_empleo, "evolucion_empleo_italia.csv")




################################################################################
################################################################################
######## "Índice de Salarios Brutos por Equivalente a Tiempo Completo" #########
################################################################################
################################################################################

# Mide la evolución de los salarios brutos por trabajador equivalente a tiempo completo
# Base 2021 = 100 - Incluye salarios + contribuciones sociales + impuestos
# Representa el costo laboral total para las empresas
# Cobertura: TOTAL INDUSTRY AND SERVICES (b to n) - Mismo sector que el índice de empleo
# Metodología: Calculado como remuneración total / número de equivalentes a tiempo completo
datos_salarios <- read_delim("Datos/Istat/Gross earnings per full time equivalent unit.csv", delim = ";")

evolucion_salarios <- datos_salarios %>%
  clean_names() %>%
  filter(econ_activity_nace_2007 == "0015") %>%  # TOTAL INDUSTRY AND SERVICES
  mutate(
    año = as.numeric(substr(time_period, 1, 4)),
    trimestre = case_when(
      grepl("Q1", time_period) ~ "Q1",
      grepl("Q2", time_period) ~ "Q2", 
      grepl("Q3", time_period) ~ "Q3",
      grepl("Q4", time_period) ~ "Q4"
    ),
    indice_salarios = as.numeric(observation)
  ) %>%
  select(año, trimestre, indice_salarios) %>%
  arrange(año, trimestre) %>%
  filter(!is.na(indice_salarios))

write_csv(evolucion_salarios, "evolucion_salarios.csv")





library(dplyr)
library(readr)
library(tidyverse)
library(janitor)

################################################################################
################################################################################
################ "Productividad Laboral por Trimestre - Italia" ###############
################################################################################
################################################################################
# Mide la contribución de cada industria al crecimiento de la productividad laboral
# Base: Porcentaje puntos - Economía total excluyendo administración pública
# Metodología: Diferencia entre tasa crecimiento valor añadido y horas trabajadas
# Cobertura: Total economía (secciones B-S NACE) - Sector más representativo
# Periodicidad: Anual (1996-2024) - Expandido a trimestres para análisis temporal
datos_productividad <- read_delim("Datos/Istat/productividad laboral por trimestre.csv", 
                                  delim = ";", 
                                  show_col_types = FALSE)

productividad_italia <- datos_productividad %>%
  clean_names() %>%
  filter(brkdw_industry_nace_rev2 == "_T") %>% 
  mutate(
    año = time_period, 
    productividad = as.numeric(observation)
  ) %>%
  select(año, productividad) %>%
  arrange(año) %>%
  filter(!is.na(productividad))

productividad_trimestral <- productividad_italia %>%
  expand(año, trimestre = c("Q1", "Q2", "Q3", "Q4")) %>%
  left_join(productividad_italia, by = "año") %>%
  select(año, trimestre, productividad) %>%
  arrange(año, trimestre)

write_csv(productividad_trimestral, "productividad_laboral_trimestral_italia.csv")


################################################################################
################################################################################
################## "Déficit/Superávit del Gobierno Italiano" ###################
################################################################################
################################################################################
# Mide la salud financiera del gobierno italiano trimestre a trimestre
# Ingresos: Impuestos, contribuciones sociales, ingresos por propiedades
# Gastos: Salarios públicos, transferencias, inversión, servicios públicos
# Déficit/Superávit: Resultado neto que determina la deuda pública acumulada
# Base: Millones de euros corrientes - Datos no ajustados estacionalmente

economic_data <- read_delim("Datos/Istat/Economic account.csv", delim = ";", 
                            locale = locale(encoding = "UTF-8"))

revenue_vs_expenditure <- economic_data %>%
  select(TIME_PERIOD, DATA_TYPE_AGGR, Aggregate, Observation) %>%
  filter(DATA_TYPE_AGGR %in% c("OTR_C_W0", "OTE_D_W0")) %>%
  mutate(
    Año = as.numeric(substr(TIME_PERIOD, 1, 4)),
    Trimestre = paste0("Q", as.numeric(substr(TIME_PERIOD, 7, 7))),  # Agregar "Q" aquí
    Value = as.numeric(Observation) * 10^6  # Convertir a millones
  ) %>%
  select(Año, Trimestre, Aggregate, Value) %>%
  pivot_wider(
    names_from = Aggregate,
    values_from = Value
  ) %>%
  mutate(
    Deficit_Surplus = `Total government revenue` - `Total Government expenditure`,
    Deficit_Surplus_Pct = (Deficit_Surplus / `Total government revenue`) * 100
  ) %>%
  arrange(Año, Trimestre)

write_csv(revenue_vs_expenditure, "revenue_vs_expenditure.csv")


################################################################################
################################################################################
############################### "TASA DE EMPLEO" ###############################
################################################################################
################################################################################

df_empleo <- read.csv("Datos/Istat/employment rate.csv", sep = ";", fileEncoding = "UTF-8")
result_empleo <- subset(df_empleo, AGE == "Y15-74", select = c(TIME_PERIOD, Observation))
result_empleo$YEAR <- as.numeric(sub("-Q.*", "", result_empleo$TIME_PERIOD))
result_empleo$QUARTER <- paste0("Q", sub(".*-Q", "", result_empleo$TIME_PERIOD))
result_empleo$EMPLEO <- as.numeric(result_empleo$Observation)  # Corregido: asignar la columna EMPLEO
result_empleo <- result_empleo[c("YEAR", "QUARTER", "EMPLEO")]

print(head(result_empleo, 10))



################################################################################
################################################################################
############################### "TASA DE DESEMPLEO" ############################
################################################################################
################################################################################

df_desempleo <- read.csv("Datos/Istat/unemployment rate.csv", sep = ";", fileEncoding = "UTF-8")
result_desempleo <- subset(df_desempleo, AGE == "Y15-74", select = c(TIME_PERIOD, Observation))
result_desempleo$YEAR <- as.numeric(sub("-Q.*", "", result_desempleo$TIME_PERIOD))
result_desempleo$QUARTER <- paste0("Q", sub(".*-Q", "", result_desempleo$TIME_PERIOD))
result_desempleo$DESEMPLEO <- as.numeric(result_desempleo$Observation)  # Corregido: asignar la columna DESEMPLEO
result_desempleo <- result_desempleo[c("YEAR", "QUARTER", "DESEMPLEO")]

print(head(result_desempleo, 10))



# Unir ambos dataframes por YEAR y QUARTER
df_unificado <- merge(result_empleo, result_desempleo, by = c("YEAR", "QUARTER"), all = TRUE)

# Ordenar por año y trimestre
df_unificado <- df_unificado[order(df_unificado$YEAR, df_unificado$QUARTER), ]

# Mostrar el dataframe unificado
print("Dataframe unificado - Tasas de empleo y desempleo:")
print(head(df_unificado, 15))
print(str(df_unificado))

write.csv(df_unificado, "Datos/definitivo_tasas_empleo_desempleo.csv")

################################################################################
################################################################################
############################### "EXPORTACIONES" ############################
################################################################################
################################################################################
df <- read.csv("Datos/Istat/exportaciones_importaciones.csv", sep = ",", fileEncoding = "UTF-8")

# Procesar exportaciones
exportaciones <- df %>%
  filter(grepl("Exports", Aggregate, ignore.case = TRUE)) %>%       
  select(TIME_PERIOD, Observation) %>%
  separate(TIME_PERIOD, into = c("anio", "trimestre"), sep = "-") %>%
  group_by(anio, trimestre) %>%
  summarise(exportaciones = sum(as.numeric(Observation), na.rm = TRUE)) %>%  # Cambiado nombre
  ungroup()

# Procesar importaciones
importaciones <- df %>%
  filter(grepl("Imports", Aggregate, ignore.case = TRUE)) %>%       
  select(TIME_PERIOD, Observation) %>%
  separate(TIME_PERIOD, into = c("anio", "trimestre"), sep = "-") %>%
  group_by(anio, trimestre) %>%
  summarise(importaciones = sum(as.numeric(Observation), na.rm = TRUE)) %>%  # Cambiado nombre
  ungroup()

# Unir ambos dataframes
comercio_exterior <- exportaciones %>%
  full_join(importaciones, by = c("anio", "trimestre")) %>%
  arrange(anio, trimestre)

# Mostrar resultado
print("Dataframe unificado de comercio exterior:")
print(head(comercio_exterior, 15))
print(str(comercio_exterior))

# Guardar como un solo CSV
write.csv(comercio_exterior, "Datos/comercio_exterior_unificado.csv")


################################################################################
################################################################################
######################## "Confianza del Consumidor - Italia" ###################
################################################################################
################################################################################

# Mide el grado de optimismo/pesimismo de los consumidores sobre la situación económica
# Base: Balance entre respuestas positivas y negativas (escala -100 a +100)
# Metodología: Encuestas a hogares sobre expectativas económicas y situación personal
# Cobertura: Toda Italia - Datos mensuales desde 1980
# Interpretación: 
#   - Valores POSITIVOS: Optimismo sobre la economía
#   - Valores NEGATIVOS: Pesimismo sobre la economía  
#   - Cero: Neutralidad

datos <- read_delim("Datos/Istat/confianza_consumidor.txt", delim = "\t")

confianza_consumidor <- datos %>%
  separate(col = 1, into = c("freq", "indic", "s_adj", "geo"), sep = ",") %>%
  filter(geo == "IT") %>%  # Filtrar solo Italia
  select(-freq, -indic, -s_adj, -geo) %>%  # Eliminar columnas innecesarias
  pivot_longer(
    cols = everything(),
    names_to = "periodo",
    values_to = "indice_confianza"
  ) %>%
  mutate(
    periodo = gsub("X", "", periodo),  # Remover "X" del nombre
    año = as.numeric(substr(periodo, 1, 4)),
    mes = as.numeric(substr(periodo, 6, 7)),
    fecha = as.Date(paste(año, mes, "01", sep = "-")),
    indice_confianza = as.numeric(indice_confianza)
  ) %>%
  filter(!is.na(indice_confianza)) %>%
  filter(año >= 1999 & año <= 2022) %>%
  select(fecha, año, mes, indice_confianza) %>%
  arrange(fecha)

confianza_consumidor_trimestral <- confianza_consumidor %>%
  mutate(
    trimestre = case_when(
      mes %in% 1:3 ~ "Q1",
      mes %in% 4:6 ~ "Q2",
      mes %in% 7:9 ~ "Q3",
      mes %in% 10:12 ~ "Q4"
    )
  ) %>%
  group_by(año, trimestre) %>%
  summarise(
    indice_confianza = mean(indice_confianza, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(año, trimestre)

write_csv(confianza_consumidor, "confianza_consumidor_mensual_italia_1999_2022.csv")
write_csv(confianza_consumidor_trimestral, "confianza_consumidor_trimestral_italia_1999_2022.csv")



################################################################################
################################################################################
############# "Índice Trimestral de Precios de Vivienda - Italia" #############
################################################################################
################################################################################
# Incluye TODOS los tipos de vivienda (nueva y existente) en Italia
# Base 2015 = 100 - Índice armonizado Eurostat
# Cobertura: Toda Italia - Datos desde 2010
# Metodología: Precios de transacción de compraventa de viviendas

datos_vivienda <- read_delim("Datos/Istat/precios_vivienda.txt", delim = "\t")

precios_vivienda_italia <- datos_vivienda %>%
  filter(grepl("IT", `freq,expend,unit,geo\\TIME_PERIOD`)) %>%
  filter(grepl("DW_ACQ,I15_Q,IT", `freq,expend,unit,geo\\TIME_PERIOD`)) %>%
  select(-`freq,expend,unit,geo\\TIME_PERIOD`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "periodo",
    values_to = "indice_precios"
  ) %>%
  mutate(
    periodo = gsub("X", "", periodo),  # Remover "X" del nombre si existe
    año = as.numeric(substr(periodo, 1, 4)),
    trimestre = paste0("Q", as.numeric(substr(periodo, 7, 7))),
    indice_precios = as.numeric(indice_precios)
  ) %>%
  filter(!is.na(indice_precios)) %>%
  select(año, trimestre, indice_precios) %>%
  arrange(año, trimestre)

write_csv(precios_vivienda_italia, "precios_vivienda_italia.csv")


################################################################################
################################################################################
####### "Tasa de Crecimiento Trimestral de Precios de Vivienda - Italia" #######
################################################################################
################################################################################
# Mide la variación trimestral de los precios de vivienda
# Base: Porcentaje - Comparación con trimestre anterior
# Indicador más sensible a cambios recientes en el mercado

tasa_crecimiento_trimestral <- datos_vivienda %>%
  filter(grepl("IT", `freq,expend,unit,geo\\TIME_PERIOD`)) %>%
  filter(grepl("DW_ACQ,RCH_Q,IT", `freq,expend,unit,geo\\TIME_PERIOD`)) %>%
  select(-`freq,expend,unit,geo\\TIME_PERIOD`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "periodo",
    values_to = "tasa_crecimiento_trimestral"
  ) %>%
  mutate(
    periodo = gsub("X", "", periodo),
    año = as.numeric(substr(periodo, 1, 4)),
    trimestre = paste0("Q", as.numeric(substr(periodo, 7, 7))),
    tasa_crecimiento_trimestral = as.numeric(tasa_crecimiento_trimestral)
  ) %>%
  filter(!is.na(tasa_crecimiento_trimestral)) %>%
  select(año, trimestre, tasa_crecimiento_trimestral) %>%
  arrange(año, trimestre)

write_csv(tasa_crecimiento_trimestral, "tasa_crecimiento_trimestral_precios_vivienda_italia.csv")








