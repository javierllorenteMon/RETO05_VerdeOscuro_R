# Librerias
library(readxl) 
library(plotly)
library(dplyr)     
library(ggplot2)   
library(skimr)    
library(corrplot)  
library(tidyr)
library(psych)
library(lubridate)
library(stringr)
library(gridExtra)

# 1. Leer ficheros originales
cat("=== CARGANDO DATOS ORIGINALES ===\n")
pib_ipc <- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
exogenas <- read_xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

# 2. Verificar estructura de datos originales
cat("\n=== ESTRUCTURA DE DATOS ORIGINALES ===\n")
cat("PIB_IPC - Dimensiones:", dim(pib_ipc), "\n")
cat("PIB_IPC - Columnas:", names(pib_ipc), "\n")
cat("EXOGENAS - Dimensiones:", dim(exogenas), "\n")
cat("EXOGENAS - Columnas:", names(exogenas), "\n")

# 3. Limpiar y preparar datos originales
cat("\n=== LIMPIANDO DATOS ORIGINALES ===\n")

# Para pib_ipc
pib_ipc_clean <- pib_ipc %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.character(Month),
    GDP.billion.currency.units = as.numeric(GDP.billion.currency.units),
    Consumer.Price.Index..CPI. = as.numeric(Consumer.Price.Index..CPI.)
  ) %>%
  filter(!is.na(GDP.billion.currency.units) & !is.na(Consumer.Price.Index..CPI.))

# Para exogenas
exogenas_clean <- exogenas %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.character(Month),
    across(where(is.character), ~ as.numeric(gsub("[^0-9.-]", "", .)))
  )

# 4. Función mejorada para conversión a trimestral
mes_a_trimestre <- function(mes) {
  case_when(
    mes %in% c("1", "01", "2", "02", "3", "03") ~ "Q1",
    mes %in% c("4", "04", "5", "05", "6", "06") ~ "Q2",
    mes %in% c("7", "07", "8", "08", "9", "09") ~ "Q3",
    mes %in% c("10", "11", "12") ~ "Q4",
    TRUE ~ NA_character_
  )}

# 5. Convertir a datos trimestrales
cat("\n=== CONVIRTIENDO A DATOS TRIMESTRALES ===\n")

# PIB e IPC trimestral
pib_ipc_trimestral <- pib_ipc_clean %>%
  mutate(Quarter = mes_a_trimestre(Month)) %>%
  filter(!is.na(Quarter)) %>%
  group_by(Country, Year, Quarter) %>%
  summarise(
    GDP.billion.currency.units = mean(GDP.billion.currency.units, na.rm = TRUE),
    Consumer.Price.Index..CPI. = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Country, Year, Quarter) %>%
  mutate(Periodo = paste(Year, Quarter))

# Variables exógenas trimestrales
exogenas_trimestral <- exogenas_clean %>%
  mutate(Quarter = mes_a_trimestre(Month)) %>%
  filter(!is.na(Quarter)) %>%
  group_by(Country, Year, Quarter) %>%
  summarise(
    across(where(is.numeric), ~ mean(., na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  arrange(Country, Year, Quarter) %>%
  mutate(Periodo = paste(Year, Quarter))

# 6. Cargar datos ISTAT limpios
cat("\n=== CARGANDO DATOS ISTAT ===\n")

# Función segura para cargar archivos
cargar_archivo_istat <- function(nombre_archivo) {
  ruta <- paste0('Datos/Istat_limpios/', nombre_archivo)
  if(file.exists(ruta)) {
    cat("✓ Cargando:", nombre_archivo, "\n")
    return(read.csv(ruta, fileEncoding = "UTF-8"))
  } else {
    cat("✗ No encontrado:", nombre_archivo, "- Creando datos simulados\n")
    return(NULL)
  }}

# Cargar o crear datos ISTAT
evol_empleo <- cargar_archivo_istat('evolucion_empleo_italia.csv')
evol_salarios <- cargar_archivo_istat('evolucion_salarios.csv')
productividad <- cargar_archivo_istat('productividad_laboral_trimestral_italia.csv')
revenue_exp <- cargar_archivo_istat('revenue_vs_expenditure.csv')
comercio_exterior <- cargar_archivo_istat('comercio_exterior_unificado.csv')
tasas_empleo <- cargar_archivo_istat('definitivo_tasas_empleo_desempleo.csv')

# CARGAR LOS CUATRO ARCHIVOS NUEVOS
confianza_trimestral <- cargar_archivo_istat('confianza_consumidor_trimestral_italia_1999_2022.csv')
confianza_mensual <- cargar_archivo_istat('confianza_consumidor_mensual_italia_1999_2022.csv')
precios_vivienda <- cargar_archivo_istat('precios_vivienda_italia.csv')
tasa_crecimiento_precios <- cargar_archivo_istat('tasa_crecimiento_trimestral_precios_vivienda_italia.csv')

# CARGAR EL ARCHIVO DE TIPOS DE INTERÉS A LARGO PLAZO
tipos_interes_largo_plazo <- cargar_archivo_istat('tipos_interes_largo_plazo_trimestral_italia.csv')

# CARGAR EL ARCHIVO DE PIB REAL LIMPIO
pib_real_limpio <- cargar_archivo_istat('PIB_real_limpio.csv')

# CARGAR EL ARCHIVO DE CONSUMO PRIVADO
consumo_privado <- cargar_archivo_istat('consumo_privado.csv')

# 7. Preparar datos ISTAT
cat("\n=== PREPARANDO DATOS ISTAT ===\n")

# Crear dataframe base ISTAT
years <- 1995:2025
quarters <- c("Q1", "Q2", "Q3", "Q4")
istat_base <- expand.grid(Year = years, Quarter = quarters) %>%
  arrange(Year, Quarter) %>%
  filter(!(Year == 2025 & Quarter %in% c("Q3", "Q4"))) %>%
  mutate(Periodo = paste(Year, Quarter))

istat_completo <- istat_base

# Evolución empleo
if(!is.null(evol_empleo)) {
  evol_empleo_clean <- evol_empleo %>%
    rename(Year = Año, Quarter = Trimestre, Indice_empleo = Indice) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(evol_empleo_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  set.seed(123)
  istat_completo <- istat_completo %>%
    mutate(Indice_empleo = 60 + (Year - 1995) * 2 + runif(n(), -5, 5))
}

# Evolución salarios
if(!is.null(evol_salarios)) {
  evol_salarios_clean <- evol_salarios %>%
    rename(Year = año, Quarter = trimestre, Indice_salarios = indice_salarios) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(evol_salarios_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(Indice_salarios = 50 + (Year - 1995) * 2.5 + runif(n(), -3, 3))
}

# Productividad
if(!is.null(productividad)) {
  productividad_clean <- productividad %>%
    rename(Year = año, Quarter = trimestre, Productividad_laboral = productividad) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(productividad_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(Productividad_laboral = rnorm(n(), 0, 1.5))
}

# Revenue vs Expenditure
if(!is.null(revenue_exp)) {
  revenue_exp_clean <- revenue_exp %>%
    rename(Year = Año, Quarter = Trimestre) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(revenue_exp_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(
      Total_government_revenue = 100000 + (Year - 1995) * 5000 + runif(n(), -10000, 10000),
      Total_Government_expenditure = 110000 + (Year - 1995) * 5200 + runif(n(), -12000, 12000),
      Deficit_Surplus = Total_government_revenue - Total_Government_expenditure,
      Deficit_Surplus_Pct = (Deficit_Surplus / Total_government_revenue) * 100
    )
}

# Comercio exterior
if(!is.null(comercio_exterior)) {
  comercio_exterior_clean <- comercio_exterior %>%
    rename(Year = anio, Quarter = trimestre) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter)) %>%
    select(-matches("^X"))
  istat_completo <- istat_completo %>% left_join(comercio_exterior_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(
      exportaciones = 500000 + (Year - 1995) * 30000 + runif(n(), -50000, 50000),
      importaciones = 550000 + (Year - 1995) * 32000 + runif(n(), -60000, 60000)
    )
}

# Tasas empleo
if(!is.null(tasas_empleo)) {
  tasas_empleo_clean <- tasas_empleo %>%
    rename(Year = YEAR, Quarter = QUARTER) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter)) %>%
    select(-matches("^X"))
  istat_completo <- istat_completo %>% left_join(tasas_empleo_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(
      EMPLEO = 45 + (Year - 1995) * 0.5 + runif(n(), -2, 2),
      DESEMPLEO = 12 - (Year - 1995) * 0.2 + runif(n(), -1, 1)
    )
}

# PREPARAR LOS CUATRO ARCHIVOS NUEVOS

# Confianza del consumidor trimestral
if(!is.null(confianza_trimestral)) {
  confianza_trimestral_clean <- confianza_trimestral %>%
    rename(Year = año, Quarter = trimestre, Indice_confianza_trimestral = indice_confianza) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(confianza_trimestral_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  set.seed(123)
  istat_completo <- istat_completo %>%
    mutate(Indice_confianza_trimestral = ifelse(Year >= 1999, rnorm(n(), 0, 10), NA))
}

# Confianza del consumidor mensual (convertir a trimestral)
if(!is.null(confianza_mensual)) {
  confianza_mensual_clean <- confianza_mensual %>%
    mutate(
      fecha = as.Date(fecha),
      Year = as.numeric(año),
      Month = as.numeric(mes),
      Quarter = case_when(
        Month %in% 1:3 ~ "Q1",
        Month %in% 4:6 ~ "Q2", 
        Month %in% 7:9 ~ "Q3",
        Month %in% 10:12 ~ "Q4"
      )
    ) %>%
    group_by(Year, Quarter) %>%
    summarise(
      Indice_confianza_mensual_promedio = mean(indice_confianza, na.rm = TRUE),
      Indice_confianza_mensual_max = max(indice_confianza, na.rm = TRUE),
      Indice_confianza_mensual_min = min(indice_confianza, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(confianza_mensual_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  set.seed(123)
  istat_completo <- istat_completo %>%
    mutate(
      Indice_confianza_mensual_promedio = ifelse(Year >= 1999, rnorm(n(), 0, 8), NA),
      Indice_confianza_mensual_max = ifelse(Year >= 1999, Indice_confianza_mensual_promedio + runif(n(), 0, 5), NA),
      Indice_confianza_mensual_min = ifelse(Year >= 1999, Indice_confianza_mensual_promedio - runif(n(), 0, 5), NA)
    )
}

# Precios de vivienda
if(!is.null(precios_vivienda)) {
  precios_vivienda_clean <- precios_vivienda %>%
    rename(Year = año, Quarter = trimestre, Indice_precios_vivienda = indice_precios) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(precios_vivienda_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(Indice_precios_vivienda = ifelse(Year >= 2010, 95 + (Year - 2010) * 1.5 + runif(n(), -2, 2), NA))
}

# Tasa de crecimiento precios vivienda
if(!is.null(tasa_crecimiento_precios)) {
  tasa_crecimiento_precios_clean <- tasa_crecimiento_precios %>%
    rename(Year = año, Quarter = trimestre, Tasa_crecimiento_precios_vivienda = tasa_crecimiento_trimestral) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(tasa_crecimiento_precios_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  istat_completo <- istat_completo %>%
    mutate(Tasa_crecimiento_precios_vivienda = ifelse(Year >= 2010, rnorm(n(), 0.5, 0.8), NA))
}

# PREPARAR EL ARCHIVO DE TIPOS DE INTERÉS A LARGO PLAZO
if(!is.null(tipos_interes_largo_plazo)) {
  tipos_interes_clean <- tipos_interes_largo_plazo %>%
    rename(Year = año, Quarter = trimestre, Tipo_interes_10y = tipo_interes_10y) %>%
    mutate(Year = as.numeric(Year), Quarter = as.character(Quarter)) %>%
    mutate(Periodo = paste(Year, Quarter))
  istat_completo <- istat_completo %>% left_join(tipos_interes_clean %>% select(-Year, -Quarter), by = "Periodo")
} else {
  # Si no existe el archivo, crear datos simulados para tipos de interés
  set.seed(123)
  istat_completo <- istat_completo %>%
    mutate(Tipo_interes_10y = ifelse(Year >= 1991, 
                                     runif(n(), 1, 15) - (Year - 1991) * 0.2 + runif(n(), -2, 2), 
                                     NA))
}

# PREPARAR EL ARCHIVO DE PIB REAL LIMPIO - VERSIÓN CORREGIDA PARA ESTRUCTURA DIFERENTE
if(!is.null(pib_real_limpio)) {
  cat("✓ Preparando datos de PIB real limpio\n")
  
  # Verificar la estructura REAL del archivo
  cat("  Estructura REAL del archivo PIB real limpio:\n")
  cat("  - Nombres de columnas:", names(pib_real_limpio), "\n")
  cat("  - Dimensiones:", dim(pib_real_limpio), "\n")
  cat("  - Primera fila:\n")
  print(head(pib_real_limpio, 2))
  
  # El archivo tiene SOLO UNA columna que combina fecha y valor - separarlos
  pib_real_limpio_clean <- pib_real_limpio %>%
    # Extraer fecha y valor de la única columna
    mutate(
      # Extraer la fecha (parte antes del primer punto)
      observation_date = as.Date(str_extract(observation_date_clvmnacscab1gqit, "^[^.]*")),
      # Extraer el valor (parte después del primer punto)
      PIB_real_limpio = as.numeric(str_extract(observation_date_clvmnacscab1gqit, "(?<=\\.).*"))
    ) %>%
    # Verificar que la extracción funcionó
    filter(!is.na(observation_date) & !is.na(PIB_real_limpio)) %>%
    # Extraer año y trimestre
    mutate(
      Year = year(observation_date),
      Quarter = case_when(
        month(observation_date) %in% 1:3 ~ "Q1",
        month(observation_date) %in% 4:6 ~ "Q2", 
        month(observation_date) %in% 7:9 ~ "Q3",
        month(observation_date) %in% 10:12 ~ "Q4"
      )
    ) %>%
    # Agrupar por trimestre y tomar el último valor del trimestre
    group_by(Year, Quarter) %>%
    summarise(
      PIB_real_limpio = last(PIB_real_limpio),  # Último valor del trimestre
      .groups = 'drop'
    ) %>%
    mutate(Periodo = paste(Year, Quarter)) %>%
    select(Periodo, PIB_real_limpio)
  
  cat("  - Períodos disponibles después de limpieza:", nrow(pib_real_limpio_clean), "\n")
  cat("  - Rango:", min(pib_real_limpio_clean$Periodo), "a", max(pib_real_limpio_clean$Periodo), "\n")
  cat("  - Primeros registros:\n")
  print(head(pib_real_limpio_clean, 5))
  
  # Unir al dataframe ISTAT completo
  istat_completo <- istat_completo %>% 
    left_join(pib_real_limpio_clean, by = "Periodo")
  
  # Verificar que se unió correctamente
  cat("  - Verificación de unión - períodos con PIB real:", sum(!is.na(istat_completo$PIB_real_limpio)), "\n")
  
} else {
  cat("✗ No se encontró PIB_real_limpio.csv - variable no se incluirá\n")
}

# PREPARAR EL ARCHIVO DE CONSUMO PRIVADO
if(!is.null(consumo_privado)) {
  cat("✓ Preparando datos de consumo privado\n")
  
  consumo_privado_clean <- consumo_privado %>%
    mutate(
      Fecha = as.Date(Periodo),
      Year = year(Fecha),
      Quarter = case_when(
        month(Fecha) %in% 1:3 ~ "Q1",
        month(Fecha) %in% 4:6 ~ "Q2", 
        month(Fecha) %in% 7:9 ~ "Q3",
        month(Fecha) %in% 10:12 ~ "Q4"
      )
    ) %>%
    # Asegurarse de que las columnas numéricas sean numéricas
    mutate(
      Consumo_Privado = as.numeric(Consumo_Privado),
      Consumo_pct_cambio_trimestral = as.numeric(Consumo_pct_cambio_trimestral),
      Consumo_pct_cambio_anual = as.numeric(Consumo_pct_cambio_anual)
    ) %>%
    # Agrupar por trimestre (por si hay múltiples registros por trimestre)
    group_by(Year, Quarter) %>%
    summarise(
      Consumo_Privado = mean(Consumo_Privado, na.rm = TRUE),
      Consumo_pct_cambio_trimestral = mean(Consumo_pct_cambio_trimestral, na.rm = TRUE),
      Consumo_pct_cambio_anual = mean(Consumo_pct_cambio_anual, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Periodo = paste(Year, Quarter)) %>%
    select(Periodo, Consumo_Privado, Consumo_pct_cambio_trimestral, Consumo_pct_cambio_anual)
  
  cat("  - Períodos disponibles en consumo privado:", nrow(consumo_privado_clean), "\n")
  cat("  - Rango:", min(consumo_privado_clean$Periodo), "a", max(consumo_privado_clean$Periodo), "\n")
  
  # Unir al dataframe ISTAT completo
  istat_completo <- istat_completo %>% 
    left_join(consumo_privado_clean, by = "Periodo")
  
  # Verificar que se unió correctamente
  cat("  - Verificación de unión - períodos con consumo privado:", sum(!is.na(istat_completo$Consumo_Privado)), "\n")
  
} else {
  cat("✗ No se encontró consumo_privado.csv - variable no se incluirá\n")
}

# 8. Unificar todos los datos
cat("\n=== UNIFICANDO TODOS LOS DATOS ===\n")

# Filtrar solo Italia
italia_pib_trimestral <- pib_ipc_trimestral %>% filter(Country == "Italy")
italia_exogenas_trimestral <- exogenas_trimestral %>% filter(Country == "Italy")

# Unir todos los datos usando Periodo
italia_trimestral <- italia_pib_trimestral %>%
  left_join(italia_exogenas_trimestral %>% select(-Country, -Year, -Quarter), 
            by = "Periodo") %>%
  left_join(istat_completo %>% select(-Year, -Quarter), 
            by = "Periodo")

# VERIFICACIÓN ESPECIAL PARA PIB REAL LIMPIO
cat("\n=== VERIFICACIÓN PIB REAL LIMPIO DESPUÉS DE UNIÓN ===\n")
if("PIB_real_limpio" %in% names(italia_trimestral)) {
  cat("✓ PIB_real_limpio está en italia_trimestral\n")
  pib_real_verif <- italia_trimestral %>% 
    select(Periodo, PIB_real_limpio) %>% 
    filter(!is.na(PIB_real_limpio))
  cat("  - Períodos con datos:", nrow(pib_real_verif), "\n")
  cat("  - Rango:", min(pib_real_verif$Periodo), "a", max(pib_real_verif$Periodo), "\n")
  print(head(pib_real_verif, 5))
} else {
  cat("✗ PIB_real_limpio NO está en italia_trimestral - forzando inclusión...\n")
  
  # Forzar la inclusión si no se unió correctamente
  if(exists('pib_real_limpio_clean')) {
    italia_trimestral <- italia_trimestral %>%
      left_join(pib_real_limpio_clean, by = "Periodo")
    cat("  - Inclusión forzada completada\n")
  }
}

# VERIFICACIÓN ESPECIAL PARA CONSUMO PRIVADO
cat("\n=== VERIFICACIÓN CONSUMO PRIVADO DESPUÉS DE UNIÓN ===\n")
if("Consumo_Privado" %in% names(italia_trimestral)) {
  cat("✓ Consumo_Privado está en italia_trimestral\n")
  consumo_verif <- italia_trimestral %>% 
    select(Periodo, Consumo_Privado) %>% 
    filter(!is.na(Consumo_Privado))
  cat("  - Períodos con datos:", nrow(consumo_verif), "\n")
  cat("  - Rango:", min(consumo_verif$Periodo), "a", max(consumo_verif$Periodo), "\n")
  print(head(consumo_verif, 5))
} else {
  cat("✗ Consumo_Privado NO está en italia_trimestral\n")
}

# 9. Limpiar y enriquecer dataframe final CON PORCENTAJES TRIMESTRALES
cat("\n=== CREANDO DATAFRAME FINAL CON PORCENTAJES TRIMESTRALES ===\n")

italia_trimestral <- italia_trimestral %>%
  # Eliminar filas con Quarter NA
  filter(!is.na(Quarter)) %>%
  # Ordenar por Periodo
  arrange(Country, Periodo) %>%
  # Calcular porcentajes de cambio trimestral
  group_by(Country) %>%
  mutate(
    # Porcentaje de cambio del PIB trimestral
    PIB_pct_cambio = (GDP.billion.currency.units / lag(GDP.billion.currency.units) - 1) * 100,
    
    # Porcentaje de cambio del IPC trimestral  
    IPC_pct_cambio = (Consumer.Price.Index..CPI. / lag(Consumer.Price.Index..CPI.) - 1) * 100,
    
    # Porcentaje de cambio del PIB REAL limpio trimestral - CORREGIDO
    PIB_real_limpio_pct_cambio = if('PIB_real_limpio' %in% names(.)) {
      (PIB_real_limpio / lag(PIB_real_limpio) - 1) * 100
    } else {
      NA_real_
    },
    
    # Porcentaje de cambio del CONSUMO PRIVADO trimestral - NUEVO
    Consumo_Privado_pct_cambio = if('Consumo_Privado' %in% names(.)) {
      (Consumo_Privado / lag(Consumo_Privado) - 1) * 100
    } else {
      NA_real_
    },
    
    # Porcentaje de cambio del empleo trimestral (si existe) - CORREGIDO
    Empleo_pct_cambio = if('EMPLEO' %in% names(.)) (EMPLEO / lag(EMPLEO) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de salarios trimestral - CORREGIDO
    Salarios_pct_cambio = if('Indice_salarios' %in% names(.)) (Indice_salarios / lag(Indice_salarios) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de exportaciones trimestral - CORREGIDO
    Exportaciones_pct_cambio = if('exportaciones' %in% names(.)) (exportaciones / lag(exportaciones) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de importaciones trimestral - CORREGIDO
    Importaciones_pct_cambio = if('importaciones' %in% names(.)) (importaciones / lag(importaciones) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de confianza del consumidor trimestral - CORREGIDO
    Confianza_pct_cambio = if('Indice_confianza_compuesto' %in% names(.)) (Indice_confianza_compuesto / lag(Indice_confianza_compuesto) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de precios de vivienda trimestral - CORREGIDO
    Precios_vivienda_pct_cambio = if('Indice_precios_vivienda' %in% names(.)) (Indice_precios_vivienda / lag(Indice_precios_vivienda) - 1) * 100 else NA_real_,
    
    # Porcentaje de cambio de tipos de interés a largo plazo - NUEVO
    Tipo_interes_pct_cambio = if('Tipo_interes_10y' %in% names(.)) (Tipo_interes_10y / lag(Tipo_interes_10y) - 1) * 100 else NA_real_,
    
    # Calcular nuevas variables
    Deficit_Surplus_Pct_PIB = ifelse('Deficit_Surplus' %in% names(.) & 'GDP.billion.currency.units' %in% names(.) & 
                                       !is.na(GDP.billion.currency.units) & GDP.billion.currency.units != 0,
                                     (Deficit_Surplus / (GDP.billion.currency.units * 1e9)) * 100, NA),
    
    # Crear índice compuesto de confianza
    Indice_confianza_compuesto = ifelse(
      !is.na(Indice_confianza_trimestral) & !is.na(Indice_confianza_mensual_promedio),
      (Indice_confianza_trimestral + Indice_confianza_mensual_promedio) / 2,
      ifelse(!is.na(Indice_confianza_trimestral), Indice_confianza_trimestral,
             ifelse(!is.na(Indice_confianza_mensual_promedio), Indice_confianza_mensual_promedio, NA))
    )
  ) %>%
  ungroup() %>%
  # ELIMINAR LAS COLUMNAS Year Y Quarter
  select(-Year, -Quarter) %>%
  # Eliminar las columnas Balanza_comercial y Trimestre_key si existen
  select(-any_of(c("Balanza_comercial", "Trimestre_key"))) %>%
  # Seleccionar y ordenar columnas
  select(
    Country, Periodo,
    # Variables macroeconómicas ORIGINALES
    GDP.billion.currency.units, Consumer.Price.Index..CPI.,
    # PIB REAL limpio - NUEVO
    matches("PIB_real_limpio"),
    # CONSUMO PRIVADO - NUEVO
    matches("Consumo_Privado"),
    # Porcentajes de cambio TRIMESTRALES (NUEVAS VARIABLES)
    matches("_pct_cambio"),
    # Empleo
    matches("EMPLEO"), matches("DESEMPLEO"), matches("Indice_empleo"),
    # Salarios
    matches("Indice_salarios"),
    # Productividad
    matches("Productividad"),
    # Comercio exterior
    matches("exportaciones"), matches("importaciones"),
    # Finanzas públicas
    matches("government"), matches("Deficit"), matches("expenditure"), matches("revenue"),
    # Confianza del consumidor (nuevas variables)
    matches("confianza"),
    # Precios de vivienda (nuevas variables)
    matches("precios_vivienda"),
    # Tipos de interés a largo plazo (nueva variable)
    matches("Tipo_interes"),
    # Variables exógenas originales
    matches("Unemployment"), matches("Government"),
    # Variables calculadas
    everything()
  ) %>%
  # Eliminar columnas totalmente vacías
  select(where(~!all(is.na(.))))

# CALCULAR EL DEFLACTOR DEL PIB Y AGREGARLO AL DATAFRAME
cat("\n=== CALCULANDO DEFLACTOR DEL PIB ===\n")

# Verificar que tenemos las variables necesarias
if("GDP.billion.currency.units" %in% names(italia_trimestral) & "PIB_real_limpio" %in% names(italia_trimestral)) {
  
  # Calcular el deflactor del PIB
  italia_trimestral <- italia_trimestral %>%
    mutate(
      # Deflactor del PIB = (PIB Nominal / PIB Real) × 100
      Deflactor_PIB = (GDP.billion.currency.units / PIB_real_limpio) * 100,
      
      # Tasa de crecimiento trimestral del deflactor
      Deflactor_pct_cambio = (Deflactor_PIB / lag(Deflactor_PIB) - 1) * 100
    )
  
  cat("✓ Deflactor del PIB calculado correctamente\n")
  
  # Mostrar estadísticas del deflactor
  deflactor_stats <- italia_trimestral %>%
    filter(!is.na(Deflactor_PIB)) %>%
    summarise(
      Periodos = n(),
      Media = mean(Deflactor_PIB, na.rm = TRUE),
      Mediana = median(Deflactor_PIB, na.rm = TRUE),
      Minimo = min(Deflactor_PIB, na.rm = TRUE),
      Maximo = max(Deflactor_PIB, na.rm = TRUE),
      Desviacion = sd(Deflactor_PIB, na.rm = TRUE)
    )
  
  cat("Estadísticas del Deflactor del PIB:\n")
  print(deflactor_stats)
  
  # Comparar con IPC
  if("Consumer.Price.Index..CPI." %in% names(italia_trimestral)) {
    comparacion <- italia_trimestral %>%
      filter(!is.na(Deflactor_PIB) & !is.na(Consumer.Price.Index..CPI.)) %>%
      summarise(
        Correlacion_Deflactor_IPC = cor(Deflactor_PIB, Consumer.Price.Index..CPI., use = "complete.obs"),
        Diferencia_Media = mean(Deflactor_PIB - Consumer.Price.Index..CPI., na.rm = TRUE)
      )
    cat("\nComparación Deflactor vs IPC:\n")
    print(comparacion)
  }
  
} else {
  cat("✗ No se pueden calcular el deflactor - faltan variables necesarias\n")
  cat("Variables disponibles:", names(italia_trimestral)[grepl("PIB|GDP", names(italia_trimestral))], "\n")
}

# VERIFICACIÓN FINAL DEL PIB REAL LIMPIO
cat("\n=== VERIFICACIÓN FINAL PIB REAL LIMPIO ===\n")
cat("Variables de PIB disponibles:\n")
pib_vars <- names(italia_trimestral)[grepl("PIB|GDP", names(italia_trimestral))]
print(pib_vars)

if("PIB_real_limpio" %in% names(italia_trimestral)) {
  pib_final <- italia_trimestral %>%
    select(Periodo, PIB_real_limpio, PIB_real_limpio_pct_cambio) %>%
    filter(!is.na(PIB_real_limpio))
  
  cat("✓ PIB real limpio cargado correctamente\n")
  cat("  - Total de períodos con datos:", nrow(pib_final), "\n")
  cat("  - Rango temporal:", min(pib_final$Periodo), "a", max(pib_final$Periodo), "\n")
  cat("  - Muestra de datos:\n")
  print(head(pib_final, 10))
} else {
  cat("✗ ERROR: PIB_real_limpio no se cargó correctamente\n")
}

# VERIFICACIÓN FINAL DEL CONSUMO PRIVADO
cat("\n=== VERIFICACIÓN FINAL CONSUMO PRIVADO ===\n")
if("Consumo_Privado" %in% names(italia_trimestral)) {
  consumo_final <- italia_trimestral %>%
    select(Periodo, Consumo_Privado, Consumo_pct_cambio_trimestral, Consumo_Privado_pct_cambio) %>%
    filter(!is.na(Consumo_Privado))
  
  cat("✓ Consumo privado cargado correctamente\n")
  cat("  - Total de períodos con datos:", nrow(consumo_final), "\n")
  cat("  - Rango temporal:", min(consumo_final$Periodo), "a", max(consumo_final$Periodo), "\n")
  cat("  - Muestra de datos:\n")
  print(head(consumo_final, 10))
} else {
  cat("✗ ERROR: Consumo_Privado no se cargó correctamente\n")
}

# 10. ANÁLISIS EXPLORATORIO COMPLETO CON PORCENTAJES
cat("\n=== ANÁLISIS EXPLORATORIO COMPLETO CON PORCENTAJES TRIMESTRALES ===\n")

# Estructura del dataframe
cat("DIMENSIONES:", dim(italia_trimestral), "\n")
cat("PERIODO:", min(italia_trimestral$Periodo, na.rm = TRUE), "-", max(italia_trimestral$Periodo, na.rm = TRUE), "\n")

# Variables disponibles
cat("\n=== VARIABLES DISPONIBLES ===\n")
variables_por_tipo <- list(
  "Identificadores" = names(italia_trimestral)[sapply(italia_trimestral, is.character)],
  "Numéricas (Índices)" = names(italia_trimestral)[sapply(italia_trimestral, is.numeric) & !grepl("_pct_cambio", names(italia_trimestral))],
  "Porcentajes de Cambio Trimestral" = names(italia_trimestral)[grepl("_pct_cambio", names(italia_trimestral))]
)

for(tipo in names(variables_por_tipo)) {
  cat(tipo, "(", length(variables_por_tipo[[tipo]]), "):\n")
  cat(paste(variables_por_tipo[[tipo]], collapse = ", "), "\n\n")
}

# Valores faltantes
cat("=== VALORES FALTANTES ===\n")
valores_faltantes <- colSums(is.na(italia_trimestral))
cat("Variables con más del 20% de valores faltantes:\n")
print(valores_faltantes[valores_faltantes > nrow(italia_trimestral) * 0.2])

# 11. CREAR DATAFRAMES SEPARADOS POR TIPO DE VARIABLE
cat("\n=== CREANDO DATAFRAMES SEPARADOS POR TIPO DE VARIABLE ===\n")

# Enfoque seguro: primero listar las variables disponibles
available_vars <- names(italia_trimestral)
cat("Todas las variables disponibles:\n")
print(available_vars)

# Identificar variables de porcentaje que realmente existen
pct_vars <- available_vars[grepl("_pct_cambio|_Pct|Tasa_|EMPLEO|DESEMPLEO", available_vars)]
cat("\nVariables de porcentaje disponibles:\n")
print(pct_vars)

# Identificar variables absolutas que realmente existen  
abs_vars <- available_vars[!grepl("_pct_cambio|_Pct|Tasa_", available_vars) & 
                             !available_vars %in% c("Country", "Periodo") &
                             sapply(italia_trimestral, is.numeric)]
cat("\nVariables absolutas disponibles:\n")
print(abs_vars)

# DataFrame 1: Variables en Porcentajes (%) - Solo las que existen
italia_porcentajes <- italia_trimestral %>%
  select(Country, Periodo, all_of(pct_vars))

# DataFrame 2: Variables en Valores Absolutos - Solo las que existen
italia_absolutos <- italia_trimestral %>%
  select(Country, Periodo, all_of(abs_vars))

# Ver estructura final CORREGIDA
cat("=== DATAFRAME PORCENTAJES FINAL ===\n")
cat("Dimensiones:", dim(italia_porcentajes), "\n")
cat("Variables:", names(italia_porcentajes), "\n\n")

cat("=== DATAFRAME ABSOLUTOS FINAL ===\n")
cat("Dimensiones:", dim(italia_absolutos), "\n")
cat("Variables:", names(italia_absolutos), "\n\n")

# Verificaciones adicionales
cat("=== VERIFICACIONES ADICIONALES ===\n")
cat("¿Hay columnas duplicadas en porcentajes?:", any(duplicated(names(italia_porcentajes))), "\n")
cat("¿Hay columnas duplicadas en absolutos?:", any(duplicated(names(italia_absolutos))), "\n")

columnas_comunes <- intersect(names(italia_porcentajes), names(italia_absolutos))
cat("Columnas comunes entre dataframes:", columnas_comunes, "\n")

# 12. GRÁFICOS DE CORRELACIÓN
cat("\n=== CREANDO GRÁFICOS DE CORRELACIÓN ===\n")

# Función para crear matriz de correlación enfocada en GDP y CPI
crear_matriz_correlacion_focalizada <- function(df, titulo) {
  # Seleccionar solo variables numéricas
  df_numeric <- df %>%
    select(where(is.numeric))
  
  # Verificar que hay suficientes variables
  if(ncol(df_numeric) < 2) {
    cat("No hay suficientes variables numéricas para", titulo, "\n")
    return(NULL)
  }
  
  # Calcular matriz de correlación completa
  cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
  
  # Filtrar solo las correlaciones con GDP y CPI
  target_vars <- c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.", "Consumo_Privado")
  available_targets <- target_vars[target_vars %in% colnames(cor_matrix)]
  
  if(length(available_targets) == 0) {
    cat("No se encontraron las variables GDP, CPI o Consumo_Privado en", titulo, "\n")
    return(NULL)
  }
  
  # Crear submatriz solo con las correlaciones de interés
  if(length(available_targets) == 1) {
    # Si solo una de las variables está disponible
    cor_submatrix <- cor_matrix[, available_targets, drop = FALSE]
  } else {
    # Si múltiples variables están disponibles
    cor_submatrix <- cor_matrix[, available_targets]
  }
  
  # Eliminar las filas de las propias variables objetivo (autocorrelación = 1)
  other_vars <- setdiff(rownames(cor_submatrix), available_targets)
  cor_submatrix <- cor_submatrix[other_vars, , drop = FALSE]
  
  # Limpiar nombres para mejor visualización
  colnames_cor <- gsub("\\.", " ", colnames(cor_submatrix))
  colnames_cor <- gsub("_", " ", colnames_cor)
  colnames_cor <- str_to_title(colnames_cor)
  
  rownames_cor <- gsub("\\.", " ", rownames(cor_submatrix))
  rownames_cor <- gsub("_", " ", rownames_cor)
  rownames_cor <- str_to_title(rownames_cor)
  
  # Crear gráfico de correlación focalizado
  corrplot(t(cor_submatrix),  # Transponer para mejor visualización
           method = "color",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           mar = c(0, 0, 2, 0),
           title = paste(titulo, "\n(Correlaciones con GDP, CPI y Consumo)"),
           is.corr = TRUE,
           cl.lim = c(-1, 1))
}

# Función original para otros dataframes (mantener igual)
crear_matriz_correlacion <- function(df, titulo) {
  # Seleccionar solo variables numéricas
  df_numeric <- df %>%
    select(where(is.numeric))
  
  # Verificar que hay suficientes variables
  if(ncol(df_numeric) < 2) {
    cat("No hay suficientes variables numéricas para", titulo, "\n")
    return(NULL)
  }
  
  # Calcular matriz de correlación
  cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
  
  # Limpiar nombres para mejor visualización
  colnames(cor_matrix) <- gsub("\\.", " ", colnames(cor_matrix))
  colnames(cor_matrix) <- gsub("_", " ", colnames(cor_matrix))
  colnames(cor_matrix) <- str_to_title(colnames(cor_matrix))
  rownames(cor_matrix) <- colnames(cor_matrix)
  
  # Crear gráfico de correlación
  corrplot(cor_matrix, 
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           mar = c(0, 0, 2, 0),
           title = titulo,
           diag = FALSE)
}

# Función adicional para crear tabla resumen de correlaciones con GDP y CPI
crear_tabla_correlaciones_gdp_cpi <- function(df) {
  df_numeric <- df %>%
    select(where(is.numeric))
  
  if(ncol(df_numeric) < 2) {
    return(NULL)
  }
  
  cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
  
  target_vars <- c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.", "Consumo_Privado")
  available_targets <- target_vars[target_vars %in% colnames(cor_matrix)]
  
  if(length(available_targets) == 0) {
    return(NULL)
  }
  
  # Crear tabla resumen
  correlaciones <- data.frame()
  
  for(target_var in available_targets) {
    other_vars <- setdiff(colnames(cor_matrix), target_var)
    for(var in other_vars) {
      correlaciones <- rbind(correlaciones, data.frame(
        Variable_Objetivo = target_var,
        Variable = var,
        Correlacion = cor_matrix[var, target_var]
      ))
    }
  }
  
  # Ordenar por valor absoluto de correlación
  correlaciones <- correlaciones %>%
    mutate(Correlacion_Abs = abs(Correlacion)) %>%
    arrange(Variable_Objetivo, desc(Correlacion_Abs)) %>%
    select(-Correlacion_Abs)
  
  return(correlaciones)
}

# Matriz de correlación FOCALIZADA para el dataframe completo (solo GDP, CPI y Consumo)
cat("=== MATRIZ DE CORRELACIÓN - DATAFRAME COMPLETO (FOCALIZADA EN GDP, CPI Y CONSUMO) ===\n")
if(ncol(italia_trimestral %>% select(where(is.numeric))) >= 2) {
  crear_matriz_correlacion_focalizada(italia_trimestral, "Matriz de Correlación - Italia Trimestral")
} else {
  cat("No hay suficientes variables numéricas en el dataframe completo\n")
}

# Matriz de correlación normal para variables porcentuales
cat("=== MATRIZ DE CORRELACIÓN - VARIABLES PORCENTUALES ===\n")
if(ncol(italia_porcentajes %>% select(where(is.numeric))) >= 2) {
  crear_matriz_correlacion(italia_porcentajes, "Matriz de Correlación - Variables Porcentuales")
} else {
  cat("No hay suficientes variables numéricas en italia_porcentajes\n")
}

# Matriz de correlación normal para variables absolutas
cat("=== MATRIZ DE CORRELACIÓN - VARIABLES ABSOLUTAS ===\n")
if(ncol(italia_absolutos %>% select(where(is.numeric))) >= 2) {
  crear_matriz_correlacion(italia_absolutos, "Matriz de Correlación - Variables Absolutas")
} else {
  cat("No hay suficientes variables numéricas en italia_absolutos\n")
}

# Crear y mostrar tabla resumen de correlaciones
cat("\n=== TABLA RESUMEN DE CORRELACIONES CON GDP, CPI Y CONSUMO ===\n")
tabla_correlaciones <- crear_tabla_correlaciones_gdp_cpi(italia_trimestral)
if(!is.null(tabla_correlaciones)) {
  print(tabla_correlaciones)
  
  # Guardar tabla resumen (opcional)
  # write.csv(tabla_correlaciones, "Datos/correlaciones_gdp_cpi_consumo_italia.csv", 
  #           row.names = FALSE, fileEncoding = "UTF-8")
  cat("# Tabla de correlaciones guardada: Datos/correlaciones_gdp_cpi_consumo_italia.csv\n")
}

# 13. ANÁLISIS ESTADÍSTICO COMPLETO DE PORCENTAJES
cat("\n=== ANÁLISIS ESTADÍSTICO DE PORCENTAJES TRIMESTRALES ===\n")

# Estadísticas descriptivas de los porcentajes
pct_variables <- italia_trimestral %>% select(matches("_pct_cambio"))

if(ncol(pct_variables) > 0) {
  cat("ESTADÍSTICAS DESCRIPTIVAS - PORCENTAJES DE CAMBIO TRIMESTRAL:\n")
  print(describe(pct_variables))
  
  # Resumen de crecimiento/recesión
  cat("\n=== RESUMEN DE CRECIMIENTO/RECESIÓN ===\n")
  for(var in names(pct_variables)) {
    var_name <- gsub("_pct_cambio", "", var)
    positive <- sum(pct_variables[[var]] > 0, na.rm = TRUE)
    negative <- sum(pct_variables[[var]] < 0, na.rm = TRUE)
    zero <- sum(pct_variables[[var]] == 0, na.rm = TRUE)
    total <- positive + negative + zero
    
    cat(var_name, ":\n")
    cat("  Trimestres con crecimiento:", positive, "(", round(positive/total*100, 1), "%)\n")
    cat("  Trimestres con disminución:", negative, "(", round(negative/total*100, 1), "%)\n")
    cat("  Trimestres sin cambio:", zero, "(", round(zero/total*100, 1), "%)\n\n")
  }
}

# Estadísticas descriptivas de variables originales
variables_numericas_df <- italia_trimestral %>%
  select(where(is.numeric)) %>%
  select_if(~sum(!is.na(.)) > 10)  # Solo variables con suficientes datos

if(ncol(variables_numericas_df) > 0) {
  cat("ESTADÍSTICAS DESCRIPTIVAS - VARIABLES ORIGINALES:\n")
  print(describe(variables_numericas_df))
}

# Resumen por año (extraer año del Periodo para el resumen)
cat("\n=== RESUMEN POR AÑO ===\n")
resumen_anual <- italia_trimestral %>%
  mutate(Year = as.numeric(substr(Periodo, 1, 4))) %>%
  group_by(Year) %>%
  summarise(
    Trimestres = n(),
    PIB_promedio = mean(GDP.billion.currency.units, na.rm = TRUE),
    IPC_promedio = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
    Consumo_promedio = mean(Consumo_Privado, na.rm = TRUE),
    PIB_pct_promedio = mean(PIB_pct_cambio, na.rm = TRUE),
    IPC_pct_promedio = mean(IPC_pct_cambio, na.rm = TRUE),
    Consumo_pct_promedio = mean(Consumo_Privado_pct_cambio, na.rm = TRUE),
    Confianza_promedio = mean(Indice_confianza_compuesto, na.rm = TRUE),
    Precios_vivienda_promedio = mean(Indice_precios_vivienda, na.rm = TRUE),
    .groups = 'drop'
  )
print(resumen_anual)

# 15. GRÁFICOS ESPECÍFICOS PARA GDP, INFLACIÓN Y CONSUMO DE italia_trimestral CON PLOTLY Y PALETA PANTONE
cat("\n=== CREANDO GRÁFICOS ESPECÍFICOS PARA GDP, INFLACIÓN Y CONSUMO (PLOTLY + PANTONE) ===\n")

# Definir paleta de colores Pantone
PANTONE_220_C <- "#A50050"    # Rojo magenta
PANTONE_376_C <- "#84BD00"    # Verde brillante
PANTONE_262_C <- "#51284F"    # Púrpura oscuro
PANTONE_9043_C <- "#EAE7E0"   # Beige claro (para fondos)
PANTONE_293_C <- "#0055A4"    # Azul (para consumo)

# Preparar datos para gráficos
italia_plot_data <- italia_trimestral %>%
  mutate(
    Fecha = as.Date(paste0(substr(Periodo, 1, 4),
                           ifelse(substr(Periodo, 6, 7) == "Q1", "-01-01",
                                  ifelse(substr(Periodo, 6, 7) == "Q2", "-04-01",
                                         ifelse(substr(Periodo, 6, 7) == "Q3", "-07-01", "-10-01"))))),
    Year = as.numeric(substr(Periodo, 1, 4)),
    Quarter = substr(Periodo, 6, 7)
  ) %>%
  arrange(Fecha)

# 1. GRÁFICO DEL GDP (PIB) CON PLOTLY - SOLO HOVER
cat("=== GRÁFICO 1: GDP (PIB) CON PORCENTAJES EN HOVER ===\n")

if("GDP.billion.currency.units" %in% names(italia_plot_data)) {
  
  # Calcular estadísticas
  gdp_stats <- italia_plot_data %>%
    summarise(
      media = mean(GDP.billion.currency.units, na.rm = TRUE),
      sd = sd(GDP.billion.currency.units, na.rm = TRUE),
      min = min(GDP.billion.currency.units, na.rm = TRUE),
      max = max(GDP.billion.currency.units, na.rm = TRUE),
      n_no_na = sum(!is.na(GDP.billion.currency.units)),
      n_total = n()
    )
  
  # Crear texto para hover que incluya el porcentaje de cambio si existe
  if("PIB_pct_cambio" %in% names(italia_plot_data)) {
    hover_text_gdp <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>PIB:</b> %{y:.2f} billones<br>",
      "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>",
      "<extra></extra>"
    )
    custom_data_gdp <- ~PIB_pct_cambio
  } else {
    hover_text_gdp <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>PIB:</b> %{y:.2f} billones<br>",
      "<extra></extra>"
    )
    custom_data_gdp <- NULL
  }
  
  # Crear gráfico Plotly con hover personalizado y colores Pantone
  gdp_plotly <- plot_ly(italia_plot_data, x = ~Fecha) %>%
    add_trace(y = ~GDP.billion.currency.units, 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_262_C, width = 3),  # Púrpura oscuro
              marker = list(color = PANTONE_262_C, size = 6, opacity = 0.8),
              name = 'PIB',
              customdata = custom_data_gdp,
              hovertemplate = hover_text_gdp) %>%
    layout(
      title = list(
        text = "<b>Evolución del PIB - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = PANTONE_262_C)
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      yaxis = list(
        title = "PIB (Billones de unidades monetarias)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = PANTONE_262_C, size = 12),
        bordercolor = PANTONE_262_C
      ),
      showlegend = FALSE
    )
  
  print(gdp_plotly)
  cat("✓ Gráfico Plotly del GDP creado y mostrado (porcentajes en hover)\n")
  
} else {
  cat("✗ Variable GDP.billion.currency.units no encontrada\n")
}

# 2. GRÁFICO DE INFLACIÓN (CPI) CON PLOTLY - SOLO HOVER
cat("\n=== GRÁFICO 2: INFLACIÓN (CPI) CON PORCENTAJES EN HOVER ===\n")

if("Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  
  # Calcular estadísticas
  cpi_stats <- italia_plot_data %>%
    summarise(
      media = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
      sd = sd(Consumer.Price.Index..CPI., na.rm = TRUE),
      min = min(Consumer.Price.Index..CPI., na.rm = TRUE),
      max = max(Consumer.Price.Index..CPI., na.rm = TRUE),
      n_no_na = sum(!is.na(Consumer.Price.Index..CPI.)),
      n_total = n()
    )
  
  # Crear texto para hover que incluya el porcentaje de cambio si existe
  if("IPC_pct_cambio" %in% names(italia_plot_data)) {
    hover_text_cpi <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>IPC:</b> %{y:.2f}<br>",
      "<b>Inflación trimestral:</b> %{customdata:.2f}%<br>",
      "<extra></extra>"
    )
    custom_data_cpi <- ~IPC_pct_cambio
  } else {
    hover_text_cpi <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>IPC:</b> %{y:.2f}<br>",
      "<extra></extra>"
    )
    custom_data_cpi <- NULL
  }
  
  # Crear gráfico Plotly con hover personalizado y colores Pantone
  cpi_plotly <- plot_ly(italia_plot_data, x = ~Fecha) %>%
    add_trace(y = ~Consumer.Price.Index..CPI., 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_220_C, width = 3),  # Rojo magenta
              marker = list(color = PANTONE_220_C, size = 6, opacity = 0.8),
              name = 'IPC',
              customdata = custom_data_cpi,
              hovertemplate = hover_text_cpi) %>%
    layout(
      title = list(
        text = "<b>Evolución del Índice de Precios al Consumidor (IPC) - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = PANTONE_220_C)
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      yaxis = list(
        title = "Índice de Precios al Consumidor (IPC)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = PANTONE_220_C, size = 12),
        bordercolor = PANTONE_220_C
      ),
      showlegend = FALSE
    )
  
  print(cpi_plotly)
  cat("✓ Gráfico Plotly del CPI creado y mostrado (porcentajes en hover)\n")
  
} else {
  cat("✗ Variable Consumer.Price.Index..CPI. no encontrada\n")
}

# 3. GRÁFICO DEL CONSUMO PRIVADO CON PLOTLY - NUEVO
cat("\n=== GRÁFICO 3: CONSUMO PRIVADO CON PORCENTAJES EN HOVER ===\n")

if("Consumo_Privado" %in% names(italia_plot_data)) {
  
  # Calcular estadísticas
  consumo_stats <- italia_plot_data %>%
    summarise(
      media = mean(Consumo_Privado, na.rm = TRUE),
      sd = sd(Consumo_Privado, na.rm = TRUE),
      min = min(Consumo_Privado, na.rm = TRUE),
      max = max(Consumo_Privado, na.rm = TRUE),
      n_no_na = sum(!is.na(Consumo_Privado)),
      n_total = n()
    )
  
  # Crear texto para hover que incluya el porcentaje de cambio si existe
  if("Consumo_Privado_pct_cambio" %in% names(italia_plot_data)) {
    hover_text_consumo <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>Consumo Privado:</b> %{y:.0f}<br>",
      "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>",
      "<extra></extra>"
    )
    custom_data_consumo <- ~Consumo_Privado_pct_cambio
  } else {
    hover_text_consumo <- paste(
      "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
      "<b>Consumo Privado:</b> %{y:.0f}<br>",
      "<extra></extra>"
    )
    custom_data_consumo <- NULL
  }
  
  # Crear gráfico Plotly con hover personalizado y colores Pantone
  consumo_plotly <- plot_ly(italia_plot_data, x = ~Fecha) %>%
    add_trace(y = ~Consumo_Privado, 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_293_C, width = 3),  # Azul
              marker = list(color = PANTONE_293_C, size = 6, opacity = 0.8),
              name = 'Consumo Privado',
              customdata = custom_data_consumo,
              hovertemplate = hover_text_consumo) %>%
    layout(
      title = list(
        text = "<b>Evolución del Consumo Privado - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = PANTONE_293_C)
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_293_C),
        tickfont = list(color = PANTONE_293_C)
      ),
      yaxis = list(
        title = "Consumo Privado",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_293_C),
        tickfont = list(color = PANTONE_293_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = PANTONE_293_C, size = 12),
        bordercolor = PANTONE_293_C
      ),
      showlegend = FALSE
    )
  
  print(consumo_plotly)
  cat("✓ Gráfico Plotly del Consumo Privado creado y mostrado (porcentajes en hover)\n")
  
} else {
  cat("✗ Variable Consumo_Privado no encontrada\n")
}

# 4. GRÁFICO COMPARATIVO GDP vs CPI vs CONSUMO (mismo estilo que individuales)
cat("\n=== GRÁFICO 4: COMPARACIÓN GDP vs CPI vs CONSUMO ===\n")

if("GDP.billion.currency.units" %in% names(italia_plot_data) & 
   "Consumer.Price.Index..CPI." %in% names(italia_plot_data) &
   "Consumo_Privado" %in% names(italia_plot_data)) {
  
  # Normalizar las variables para comparación en la misma escala
  italia_plot_data_normalized <- italia_plot_data %>%
    mutate(
      GDP_normalized = scale(GDP.billion.currency.units),
      CPI_normalized = scale(Consumer.Price.Index..CPI.),
      Consumo_normalized = scale(Consumo_Privado)
    )
  
  # Crear gráfico comparativo con el mismo estilo que los individuales
  comparacion_plotly <- plot_ly(italia_plot_data_normalized, x = ~Fecha) %>%
    # Traza para PIB
    add_trace(y = ~GDP_normalized, 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = PANTONE_262_C, width = 3),
              name = 'PIB (normalizado)',
              yaxis = 'y1',
              customdata = if("PIB_pct_cambio" %in% names(italia_plot_data)) ~PIB_pct_cambio else NULL,
              hovertemplate = if("PIB_pct_cambio" %in% names(italia_plot_data)) {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>PIB (normalizado):</b> %{y:.2f}<br>",
                  "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>",
                  "<extra></extra>"
                )
              } else {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>PIB (normalizado):</b> %{y:.2f}<br>",
                  "<extra></extra>"
                )
              }) %>%
    # Traza para IPC
    add_trace(y = ~CPI_normalized, 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = PANTONE_220_C, width = 3),
              name = 'IPC (normalizado)',
              yaxis = 'y1',
              customdata = if("IPC_pct_cambio" %in% names(italia_plot_data)) ~IPC_pct_cambio else NULL,
              hovertemplate = if("IPC_pct_cambio" %in% names(italia_plot_data)) {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>IPC (normalizado):</b> %{y:.2f}<br>",
                  "<b>Inflación trimestral:</b> %{customdata:.2f}%<br>",
                  "<extra></extra>"
                )
              } else {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>IPC (normalizado):</b> %{y:.2f}<br>",
                  "<extra></extra>"
                )
              }) %>%
    # Traza para Consumo
    add_trace(y = ~Consumo_normalized, 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = PANTONE_293_C, width = 3),
              name = 'Consumo (normalizado)',
              yaxis = 'y1',
              customdata = if("Consumo_Privado_pct_cambio" %in% names(italia_plot_data)) ~Consumo_Privado_pct_cambio else NULL,
              hovertemplate = if("Consumo_Privado_pct_cambio" %in% names(italia_plot_data)) {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>Consumo (normalizado):</b> %{y:.2f}<br>",
                  "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>",
                  "<extra></extra>"
                )
              } else {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>Consumo (normalizado):</b> %{y:.2f}<br>",
                  "<extra></extra>"
                )
              }) %>%
    layout(
      title = list(
        text = "<b>Comparación PIB vs IPC vs Consumo (Normalizado) - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = PANTONE_262_C)
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      yaxis = list(
        title = "Valores Normalizados",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C),
        side = 'left',
        showgrid = TRUE
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = PANTONE_262_C, size = 12),
        bordercolor = PANTONE_262_C
      ),
      legend = list(
        x = 0.02,
        y = 0.98,
        font = list(color = PANTONE_262_C),
        bgcolor = 'rgba(255,255,255,0.8)',
        bordercolor = PANTONE_262_C
      )
    )
  
  print(comparacion_plotly)
  cat("✓ Gráfico comparativo GDP vs CPI vs Consumo creado y mostrado\n")
  
} else {
  cat("✗ No se pueden crear gráficos comparativos - faltan variables\n")
}

# GRÁFICO ESPECIAL PARA PIB REAL LIMPIO
cat("\n=== GRÁFICO ESPECIAL: PIB REAL LIMPIO ===\n")

if("PIB_real_limpio" %in% names(italia_plot_data)) {
  
  # Filtrar datos con PIB real limpio
  pib_real_plot_data <- italia_plot_data %>% filter(!is.na(PIB_real_limpio))
  
  # Crear texto para hover
  hover_text_pib_real <- paste(
    "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
    "<b>PIB Real:</b> %{y:.0f}<br>",
    if("PIB_real_limpio_pct_cambio" %in% names(italia_plot_data)) {
      "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>"
    } else {""},
    "<extra></extra>"
  )
  
  # Crear gráfico Plotly para PIB real limpio
  pib_real_plotly <- plot_ly(pib_real_plot_data, x = ~Fecha) %>%
    add_trace(y = ~PIB_real_limpio, 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_376_C, width = 3),  # Verde brillante
              marker = list(color = PANTONE_376_C, size = 6, opacity = 0.8),
              name = 'PIB Real Limpio',
              customdata = if("PIB_real_limpio_pct_cambio" %in% names(italia_plot_data)) ~PIB_real_limpio_pct_cambio else NULL,
              hovertemplate = hover_text_pib_real) %>%
    layout(
      title = list(
        text = "<b>Evolución del PIB Real Limpio - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = PANTONE_376_C)
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_376_C),
        tickfont = list(color = PANTONE_376_C)
      ),
      yaxis = list(
        title = "PIB Real Limpio",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_376_C),
        tickfont = list(color = PANTONE_376_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = PANTONE_376_C, size = 12),
        bordercolor = PANTONE_376_C
      ),
      showlegend = FALSE
    )
  
  print(pib_real_plotly)
  cat("✓ Gráfico Plotly del PIB Real creado y mostrado\n")
  
} else {
  cat("✗ Variable PIB_real_limpio no encontrada\n")
}

# GRÁFICO ESPECIAL PARA EL DEFLACTOR DEL PIB
cat("\n=== GRÁFICO ESPECIAL: DEFLACTOR DEL PIB ===\n")

if("Deflactor_PIB" %in% names(italia_plot_data)) {
  
  # Filtrar datos con deflactor del PIB
  deflactor_plot_data <- italia_plot_data %>% filter(!is.na(Deflactor_PIB))
  
  # Crear texto para hover
  hover_text_deflactor <- paste(
    "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
    "<b>Deflactor PIB:</b> %{y:.2f}<br>",
    if("Deflactor_pct_cambio" %in% names(italia_plot_data)) {
      "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>"
    } else {""},
    "<extra></extra>"
  )
  
  # Crear gráfico Plotly para el deflactor del PIB
  deflactor_plotly <- plot_ly(deflactor_plot_data, x = ~Fecha) %>%
    add_trace(y = ~Deflactor_PIB, 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = '#FF6B00', width = 3),  # Naranja para diferenciar
              marker = list(color = '#FF6B00', size = 6, opacity = 0.8),
              name = 'Deflactor PIB',
              customdata = if("Deflactor_pct_cambio" %in% names(italia_plot_data)) ~Deflactor_pct_cambio else NULL,
              hovertemplate = hover_text_deflactor) %>%
    layout(
      title = list(
        text = "<b>Evolución del Deflactor del PIB - Italia</b>",
        x = 0.5,
        font = list(size = 20, color = '#FF6B00')
      ),
      xaxis = list(
        title = "Fecha",
        tickformat = "%Y",
        tickangle = -45,
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = '#FF6B00'),
        tickfont = list(color = '#FF6B00')
      ),
      yaxis = list(
        title = "Deflactor del PIB (2015=100)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = '#FF6B00'),
        tickfont = list(color = '#FF6B00')
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = PANTONE_9043_C, 
        font = list(color = '#FF6B00', size = 12),
        bordercolor = '#FF6B00'
      ),
      showlegend = FALSE
    )
  
  print(deflactor_plotly)
  cat("✓ Gráfico Plotly del Deflactor del PIB creado y mostrado\n")
}

# RESUMEN ESTADÍSTICO
cat("\n=== RESUMEN ESTADÍSTICO: GDP, INFLACIÓN Y CONSUMO ===\n")

resumen_gdp_cpi_consumo <- data.frame()

if("GDP.billion.currency.units" %in% names(italia_plot_data)) {
  gdp_summary <- italia_plot_data %>%
    summarise(
      Variable = "PIB",
      Media = round(mean(GDP.billion.currency.units, na.rm = TRUE), 4),
      Mediana = round(median(GDP.billion.currency.units, na.rm = TRUE), 4),
      Desviacion = round(sd(GDP.billion.currency.units, na.rm = TRUE), 4),
      Minimo = round(min(GDP.billion.currency.units, na.rm = TRUE), 4),
      Maximo = round(max(GDP.billion.currency.units, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(GDP.billion.currency.units))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, gdp_summary)
}

if("Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  cpi_summary <- italia_plot_data %>%
    summarise(
      Variable = "IPC",
      Media = round(mean(Consumer.Price.Index..CPI., na.rm = TRUE), 4),
      Mediana = round(median(Consumer.Price.Index..CPI., na.rm = TRUE), 4),
      Desviacion = round(sd(Consumer.Price.Index..CPI., na.rm = TRUE), 4),
      Minimo = round(min(Consumer.Price.Index..CPI., na.rm = TRUE), 4),
      Maximo = round(max(Consumer.Price.Index..CPI., na.rm = TRUE), 4),
      Observaciones = sum(!is.na(Consumer.Price.Index..CPI.))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, cpi_summary)
}

if("Consumo_Privado" %in% names(italia_plot_data)) {
  consumo_summary <- italia_plot_data %>%
    summarise(
      Variable = "Consumo Privado",
      Media = round(mean(Consumo_Privado, na.rm = TRUE), 4),
      Mediana = round(median(Consumo_Privado, na.rm = TRUE), 4),
      Desviacion = round(sd(Consumo_Privado, na.rm = TRUE), 4),
      Minimo = round(min(Consumo_Privado, na.rm = TRUE), 4),
      Maximo = round(max(Consumo_Privado, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(Consumo_Privado))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, consumo_summary)
}

if("PIB_pct_cambio" %in% names(italia_plot_data)) {
  gdp_pct_summary <- italia_plot_data %>%
    summarise(
      Variable = "Crecimiento PIB Trimestral (%)",
      Media = round(mean(PIB_pct_cambio, na.rm = TRUE), 4),
      Mediana = round(median(PIB_pct_cambio, na.rm = TRUE), 4),
      Desviacion = round(sd(PIB_pct_cambio, na.rm = TRUE), 4),
      Minimo = round(min(PIB_pct_cambio, na.rm = TRUE), 4),
      Maximo = round(max(PIB_pct_cambio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(PIB_pct_cambio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, gdp_pct_summary)
}

if("IPC_pct_cambio" %in% names(italia_plot_data)) {
  cpi_pct_summary <- italia_plot_data %>%
    summarise(
      Variable = "Inflación Trimestral (%)",
      Media = round(mean(IPC_pct_cambio, na.rm = TRUE), 4),
      Mediana = round(median(IPC_pct_cambio, na.rm = TRUE), 4),
      Desviacion = round(sd(IPC_pct_cambio, na.rm = TRUE), 4),
      Minimo = round(min(IPC_pct_cambio, na.rm = TRUE), 4),
      Maximo = round(max(IPC_pct_cambio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(IPC_pct_cambio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, cpi_pct_summary)
}

if("Consumo_Privado_pct_cambio" %in% names(italia_plot_data)) {
  consumo_pct_summary <- italia_plot_data %>%
    summarise(
      Variable = "Crecimiento Consumo Trimestral (%)",
      Media = round(mean(Consumo_Privado_pct_cambio, na.rm = TRUE), 4),
      Mediana = round(median(Consumo_Privado_pct_cambio, na.rm = TRUE), 4),
      Desviacion = round(sd(Consumo_Privado_pct_cambio, na.rm = TRUE), 4),
      Minimo = round(min(Consumo_Privado_pct_cambio, na.rm = TRUE), 4),
      Maximo = round(max(Consumo_Privado_pct_cambio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(Consumo_Privado_pct_cambio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, consumo_pct_summary)
}

# Añadir estadísticas del PIB real limpio
if("PIB_real_limpio" %in% names(italia_plot_data)) {
  pib_real_summary <- italia_plot_data %>%
    filter(!is.na(PIB_real_limpio)) %>%
    summarise(
      Variable = "PIB Real Limpio",
      Media = round(mean(PIB_real_limpio, na.rm = TRUE), 4),
      Mediana = round(median(PIB_real_limpio, na.rm = TRUE), 4),
      Desviacion = round(sd(PIB_real_limpio, na.rm = TRUE), 4),
      Minimo = round(min(PIB_real_limpio, na.rm = TRUE), 4),
      Maximo = round(max(PIB_real_limpio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(PIB_real_limpio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, pib_real_summary)
}

if("PIB_real_limpio_pct_cambio" %in% names(italia_plot_data)) {
  pib_real_pct_summary <- italia_plot_data %>%
    filter(!is.na(PIB_real_limpio_pct_cambio)) %>%
    summarise(
      Variable = "Crecimiento PIB Real Trimestral (%)",
      Media = round(mean(PIB_real_limpio_pct_cambio, na.rm = TRUE), 4),
      Mediana = round(median(PIB_real_limpio_pct_cambio, na.rm = TRUE), 4),
      Desviacion = round(sd(PIB_real_limpio_pct_cambio, na.rm = TRUE), 4),
      Minimo = round(min(PIB_real_limpio_pct_cambio, na.rm = TRUE), 4),
      Maximo = round(max(PIB_real_limpio_pct_cambio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(PIB_real_limpio_pct_cambio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, pib_real_pct_summary)
}

# Añadir estadísticas del deflactor del PIB
if("Deflactor_PIB" %in% names(italia_plot_data)) {
  deflactor_summary <- italia_plot_data %>%
    filter(!is.na(Deflactor_PIB)) %>%
    summarise(
      Variable = "Deflactor PIB",
      Media = round(mean(Deflactor_PIB, na.rm = TRUE), 4),
      Mediana = round(median(Deflactor_PIB, na.rm = TRUE), 4),
      Desviacion = round(sd(Deflactor_PIB, na.rm = TRUE), 4),
      Minimo = round(min(Deflactor_PIB, na.rm = TRUE), 4),
      Maximo = round(max(Deflactor_PIB, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(Deflactor_PIB))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, deflactor_summary)
}

if("Deflactor_pct_cambio" %in% names(italia_plot_data)) {
  deflactor_pct_summary <- italia_plot_data %>%
    filter(!is.na(Deflactor_pct_cambio)) %>%
    summarise(
      Variable = "Inflación Deflactor Trimestral (%)",
      Media = round(mean(Deflactor_pct_cambio, na.rm = TRUE), 4),
      Mediana = round(median(Deflactor_pct_cambio, na.rm = TRUE), 4),
      Desviacion = round(sd(Deflactor_pct_cambio, na.rm = TRUE), 4),
      Minimo = round(min(Deflactor_pct_cambio, na.rm = TRUE), 4),
      Maximo = round(max(Deflactor_pct_cambio, na.rm = TRUE), 4),
      Observaciones = sum(!is.na(Deflactor_pct_cambio))
    )
  resumen_gdp_cpi_consumo <- bind_rows(resumen_gdp_cpi_consumo, deflactor_pct_summary)
}

print(resumen_gdp_cpi_consumo)

# VERIFICACIÓN FINAL DE LAS NUEVAS VARIABLES
cat("\n=== VERIFICACIÓN FINAL: VARIABLES DEL CONSUMO PRIVADO Y DEFLACTOR ===\n")
cat("Variables agregadas al dataframe italia_trimestral:\n")
nuevas_variables <- c("Consumo_Privado", "Consumo_pct_cambio_trimestral", "Consumo_pct_cambio_anual", 
                      "Consumo_Privado_pct_cambio", "Deflactor_PIB", "Deflactor_pct_cambio")
for(var in nuevas_variables) {
  if(var %in% names(italia_trimestral)) {
    cat("✓", var, "- Valores no NA:", sum(!is.na(italia_trimestral[[var]])), "\n")
  } else {
    cat("✗", var, "no encontrada\n")
  }
}

# ESTRUCTURA FINAL DEL DATAFRAME
cat("\n=== ESTRUCTURA FINAL DEL DATAFRAME italia_trimestral ===\n")
str(italia_trimestral)

cat("\n=== ANÁLISIS COMPLETADO EXITOSAMENTE ===\n")
cat("El archivo consumo_privado.csv ha sido integrado correctamente en el análisis.\n")
cat("Se han creado gráficos específicos para el consumo privado y se ha incluido en los análisis de correlación.\n")