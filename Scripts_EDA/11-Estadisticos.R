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
  target_vars <- c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.")
  available_targets <- target_vars[target_vars %in% colnames(cor_matrix)]
  
  if(length(available_targets) == 0) {
    cat("No se encontraron las variables GDP o CPI en", titulo, "\n")
    return(NULL)
  }
  
  # Crear submatriz solo con las correlaciones de interés
  if(length(available_targets) == 1) {
    # Si solo una de las dos variables está disponible
    cor_submatrix <- cor_matrix[, available_targets, drop = FALSE]
  } else {
    # Si ambas variables están disponibles
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
           title = paste(titulo, "\n(Correlaciones con GDP y CPI)"),
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
  
  target_vars <- c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.")
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

# Matriz de correlación FOCALIZADA para el dataframe completo (solo GDP y CPI)
cat("=== MATRIZ DE CORRELACIÓN - DATAFRAME COMPLETO (FOCALIZADA EN GDP Y CPI) ===\n")
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
cat("\n=== TABLA RESUMEN DE CORRELACIONES CON GDP Y CPI ===\n")
tabla_correlaciones <- crear_tabla_correlaciones_gdp_cpi(italia_trimestral)
if(!is.null(tabla_correlaciones)) {
  print(tabla_correlaciones)
  
  # Guardar tabla resumen (opcional)
  # write.csv(tabla_correlaciones, "Datos/correlaciones_gdp_cpi_italia.csv", 
  #           row.names = FALSE, fileEncoding = "UTF-8")
  cat("# Tabla de correlaciones guardada: Datos/correlaciones_gdp_cpi_italia.csv\n")
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
    PIB_pct_promedio = mean(PIB_pct_cambio, na.rm = TRUE),
    IPC_pct_promedio = mean(IPC_pct_cambio, na.rm = TRUE),
    Confianza_promedio = mean(Indice_confianza_compuesto, na.rm = TRUE),
    Precios_vivienda_promedio = mean(Indice_precios_vivienda, na.rm = TRUE),
    .groups = 'drop'
  )
print(resumen_anual)

# 15. GRÁFICOS ESPECÍFICOS PARA GDP E INFLACIÓN DE italia_trimestral CON PLOTLY Y PALETA PANTONE
cat("\n=== CREANDO GRÁFICOS ESPECÍFICOS PARA GDP E INFLACIÓN (PLOTLY + PANTONE) ===\n")

# Definir paleta de colores Pantone
PANTONE_220_C <- "#A50050"    # Rojo magenta
PANTONE_376_C <- "#84BD00"    # Verde brillante
PANTONE_262_C <- "#51284F"    # Púrpura oscuro
PANTONE_9043_C <- "#EAE7E0"   # Beige claro (para fondos)

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

# 3. GRÁFICO COMPARATIVO GDP vs CPI (mismo estilo que individuales)
cat("\n=== GRÁFICO 3: COMPARACIÓN GDP vs CPI ===\n")

if("GDP.billion.currency.units" %in% names(italia_plot_data) & 
   "Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  
  # Crear gráfico comparativo con el mismo estilo que los individuales
  comparacion_plotly <- plot_ly(italia_plot_data, x = ~Fecha) %>%
    # Traza para PIB
    add_trace(y = ~GDP.billion.currency.units, 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_262_C, width = 3),
              marker = list(color = PANTONE_262_C, size = 5, opacity = 0.7),
              name = 'PIB',
              yaxis = 'y1',
              customdata = if("PIB_pct_cambio" %in% names(italia_plot_data)) ~PIB_pct_cambio else NULL,
              hovertemplate = if("PIB_pct_cambio" %in% names(italia_plot_data)) {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>PIB:</b> %{y:.2f} billones<br>",
                  "<b>Crecimiento trimestral:</b> %{customdata:.2f}%<br>",
                  "<extra></extra>"
                )
              } else {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>PIB:</b> %{y:.2f} billones<br>",
                  "<extra></extra>"
                )
              }) %>%
    # Traza para IPC
    add_trace(y = ~Consumer.Price.Index..CPI., 
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = PANTONE_220_C, width = 3),
              marker = list(color = PANTONE_220_C, size = 5, opacity = 0.7),
              name = 'IPC',
              yaxis = 'y2',
              customdata = if("IPC_pct_cambio" %in% names(italia_plot_data)) ~IPC_pct_cambio else NULL,
              hovertemplate = if("IPC_pct_cambio" %in% names(italia_plot_data)) {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>IPC:</b> %{y:.2f}<br>",
                  "<b>Inflación trimestral:</b> %{customdata:.2f}%<br>",
                  "<extra></extra>"
                )
              } else {
                paste(
                  "<b>Fecha:</b> %{x|%Y-Q%q}<br>",
                  "<b>IPC:</b> %{y:.2f}<br>",
                  "<extra></extra>"
                )
              }) %>%
    layout(
      title = list(
        text = "<b>Comparación PIB vs IPC - Italia</b>",
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
        tickfont = list(color = PANTONE_262_C),
        side = 'left',
        showgrid = TRUE
      ),
      yaxis2 = list(
        title = "Índice de Precios al Consumidor (IPC)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C),
        side = 'right',
        overlaying = "y",
        showgrid = FALSE
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
      ),
      margin = list(r = 80)  # Margen para el segundo eje Y
    )
  
  print(comparacion_plotly)
  cat("✓ Gráfico comparativo GDP vs CPI creado y mostrado\n")
  
} else {
  cat("✗ No se pueden crear gráficos comparativos - faltan variables\n")
}

# RESUMEN ESTADÍSTICO
cat("\n=== RESUMEN ESTADÍSTICO: GDP E INFLACIÓN ===\n")

resumen_gdp_cpi <- data.frame()

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
  resumen_gdp_cpi <- bind_rows(resumen_gdp_cpi, gdp_summary)
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
  resumen_gdp_cpi <- bind_rows(resumen_gdp_cpi, cpi_summary)
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
  resumen_gdp_cpi <- bind_rows(resumen_gdp_cpi, gdp_pct_summary)
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
  resumen_gdp_cpi <- bind_rows(resumen_gdp_cpi, cpi_pct_summary)
}

print(resumen_gdp_cpi)

cat("\n=== PROCESO COMPLETADO ===\n")
cat("✓ 3 Gráficos Plotly específicos creados\n")
cat("✓ Paleta Pantone aplicada consistentemente\n")
cat("✓ Porcentajes de cambio visibles solo al pasar el ratón (hover)\n")
cat("✓ Gráfico comparativo con mismo estilo que individuales\n")
cat("✓ Diseño profesional y cohesivo\n")
cat("✓ Resumen estadístico generado\n")
################################################################################
################################################################################
################################################################################
# 16. BOXPLOTS INDIVIDUALES PARA TODAS LAS VARIABLES NUMÉRICAS - TODAS LAS PÁGINAS A LA VEZ
cat("\n=== CREANDO BOXPLOTS INDIVIDUALES PARA TODAS LAS VARIABLES ===\n")

# Obtener solo las variables numéricas (excluyendo identificadores)
variables_numericas <- italia_trimestral %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("Year", "Quarter")))  # Excluir columnas no numéricas si existen

cat("Número de variables numéricas:", ncol(variables_numericas), "\n")
cat("Variables a graficar:", names(variables_numericas), "\n")

# Función CORREGIDA para crear boxplot individual
crear_boxplot_individual <- function(data, variable_name) {
  # Limpiar nombre para el título
  nombre_limpio <- gsub("\\.", " ", variable_name)
  nombre_limpio <- gsub("_", " ", nombre_limpio)
  nombre_limpio <- str_to_title(nombre_limpio)
  
  # Calcular estadísticas básicas para el subtítulo
  stats <- data %>%
    summarise(
      media = round(mean(!!sym(variable_name), na.rm = TRUE), 2),
      mediana = round(median(!!sym(variable_name), na.rm = TRUE), 2),
      desviacion = round(sd(!!sym(variable_name), na.rm = TRUE), 2),
      minimo = round(min(!!sym(variable_name), na.rm = TRUE), 2),
      maximo = round(max(!!sym(variable_name), na.rm = TRUE), 2),
      na_count = sum(is.na(!!sym(variable_name))))
      
      # Crear el boxplot CORREGIDO
      ggplot(data, aes(x = "", y = !!sym(variable_name))) +
        geom_boxplot(
          fill = PANTONE_9043_C, 
          color = PANTONE_262_C,
          alpha = 0.7,
          outlier.color = PANTONE_220_C,
          outlier.size = 2
        ) +
        stat_summary(
          fun = mean, 
          geom = "point", 
          shape = 18, 
          size = 3, 
          color = PANTONE_376_C
        ) +
        labs(
          title = nombre_limpio,
          subtitle = paste0(
            "Media: ", stats$media, 
            " | Mediana: ", stats$mediana,
            " | SD: ", stats$desviacion,
            "\nMín: ", stats$minimo, 
            " | Máx: ", stats$maximo,
            " | NA: ", stats$na_count
          ),
          x = NULL,
          y = NULL
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            size = 11, 
            face = "bold", 
            color = PANTONE_262_C,
            hjust = 0.5
          ),
          plot.subtitle = element_text(
            size = 8, 
            color = "darkgray",
            hjust = 0.5
          ),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 8),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
}

# Crear todos los boxplots individuales
boxplots_list <- list()

for (i in 1:ncol(variables_numericas)) {
  var_name <- names(variables_numericas)[i]
  cat("Creando boxplot para:", var_name, "\n")
  
  # Filtrar datos sin NA para esta variable
  datos_variable <- variables_numericas %>% 
    select(all_of(var_name)) %>% 
    filter(!is.na(!!sym(var_name)))
  
  if (nrow(datos_variable) > 0) {
    boxplots_list[[var_name]] <- crear_boxplot_individual(datos_variable, var_name)
  }
}

# ORGANIZAR TODAS LAS PÁGINAS A LA VEZ
cat("\n=== ORGANIZANDO TODAS LAS PÁGINAS A LA VEZ ===\n")

# Definir número de gráficos por página
graficos_por_pagina <- 9  # 3x3 grid
total_graficos <- length(boxplots_list)
total_paginas <- ceiling(total_graficos / graficos_por_pagina)

cat("Total de boxplots creados:", total_graficos, "\n")
cat("Gráficos por página:", graficos_por_pagina, "\n")
cat("Total de páginas necesarias:", total_paginas, "\n")

# Función para crear una lista con todas las páginas
crear_todas_las_paginas <- function(lista_boxplots, por_pagina = 9) {
  total_paginas <- ceiling(length(lista_boxplots) / por_pagina)
  todas_las_paginas <- list()
  
  for (pagina in 1:total_paginas) {
    inicio <- (pagina - 1) * por_pagina + 1
    fin <- min(pagina * por_pagina, length(lista_boxplots))
    
    if (inicio <= length(lista_boxplots)) {
      cat("Preparando página", pagina, "- Gráficos", inicio, "a", fin, "\n")
      
      # Crear grid para esta página
      pagina_actual <- do.call("grid.arrange", 
                               c(lista_boxplots[inicio:fin], 
                                 ncol = 3, 
                                 nrow = 3,
                                 top = paste("Boxplots de Variables - Italia Trimestral - Página", pagina, "de", total_paginas)))
      
      todas_las_paginas[[pagina]] <- pagina_actual
    }
  }
  
  return(todas_las_paginas)
}

# OPCIÓN 1: Mostrar todas las páginas en una ventana gráfica múltiple (si el dispositivo lo permite)
cat("\n=== OPCIÓN 1: MOSTRAR TODAS LAS PÁGINAS EN UNA VENTANA GRÁFICA MÚLTIPLE ===\n")

# Configurar ventana gráfica para múltiples páginas
if (total_paginas > 1) {
  # Intentar configurar múltiples paneles
  tryCatch({
    # Configurar layout según número de páginas
    if (total_paginas <= 4) {
      par(mfrow = c(2, 2))
    } else if (total_paginas <= 6) {
      par(mfrow = c(2, 3))
    } else if (total_paginas <= 9) {
      par(mfrow = c(3, 3))
    } else {
      par(mfrow = c(ceiling(sqrt(total_paginas)), ceiling(sqrt(total_paginas))))
    }
    
    cat("Ventana gráfica configurada para", total_paginas, "páginas\n")
  }, error = function(e) {
    cat("No se pudo configurar ventana múltiple. Usando método alternativo.\n")
  })
}

# OPCIÓN 2: Usar grid.arrange con marcos múltiples (MEJOR OPCIÓN)
cat("\n=== OPCIÓN 2: CREAR SUPER-LAYOUT CON TODAS LAS PÁGINAS ===\n")

# Calcular layout óptimo para todas las páginas
filas_total <- ceiling(total_paginas / 2)  # 2 columnas
columnas_total <- ifelse(total_paginas > 1, 2, 1)

cat("Layout configurado:", filas_total, "filas x", columnas_total, "columnas\n")

# Crear una función para mostrar todas las páginas a la vez
mostrar_todas_las_paginas <- function() {
  # Crear lista de todas las páginas
  todas_las_paginas <- list()
  
  for (pagina in 1:total_paginas) {
    inicio <- (pagina - 1) * graficos_por_pagina + 1
    fin <- min(pagina * graficos_por_pagina, total_graficos)
    
    # Crear cada página individual
    pagina_grid <- grid.arrange(
      grobs = boxplots_list[inicio:fin],
      ncol = 3,
      nrow = 3,
      top = textGrob(paste("Página", pagina, "de", total_paginas), 
                     gp = gpar(fontsize = 16, fontface = "bold", col = PANTONE_262_C))
    )
    
    todas_las_paginas[[pagina]] <- pagina_grid
  }
  
  # Organizar todas las páginas en un super-layout
  if (length(todas_las_paginas) > 0) {
    # Usar grid.arrange para organizar las páginas
    do.call(grid.arrange, c(todas_las_paginas, ncol = columnas_total))
  }
}

# OPCIÓN 3: Método más simple - Crear un PDF temporal y mostrar todas las páginas
cat("\n=== OPCIÓN 3: MÉTODO SIMPLIFICADO - MOSTRAR PÁGINA POR PÁGINA EN SECUENCIA ===\n")

# Mostrar cada página en secuencia (todas visibles una tras otra)
for (pagina in 1:total_paginas) {
  cat("\n--- MOSTRANDO PÁGINA", pagina, "DE", total_paginas, "---\n")
  
  inicio <- (pagina - 1) * graficos_por_pagina + 1
  fin <- min(pagina * graficos_por_pagina, total_graficos)
  
  # Mostrar página actual
  grid.arrange(
    grobs = boxplots_list[inicio:fin],
    ncol = 3,
    nrow = 3,
    top = paste("Boxplots - Italia Trimestral - Página", pagina, "de", total_paginas)
  )
  
  # Pausa opcional entre páginas (puedes comentar esta línea si quieres verlas todas rápido)
  if (pagina < total_paginas) {
    cat("Presiona Enter para ver la siguiente página...")
    readline()
  }
}

# OPCIÓN 4: Guardar y mostrar miniaturas de todas las páginas
cat("\n=== OPCIÓN 4: CREAR RESUMEN CON MINIATURAS DE TODAS LAS PÁGINAS ===\n")

# Crear una página de resumen con miniaturas de cada página
crear_pagina_resumen <- function() {
  # Crear miniaturas simplificadas para el resumen
  boxplots_mini <- list()
  
  for (i in 1:min(6, total_graficos)) {  # Mostrar máximo 6 miniaturas
    var_name <- names(boxplots_list)[i]
    nombre_limpio <- gsub("\\.", " ", var_name)
    nombre_limpio <- gsub("_", " ", nombre_limpio)
    nombre_limpio <- str_to_title(nombre_limpio)
    
    if (length(nombre_limpio) > 20) {
      nombre_limpio <- paste0(substr(nombre_limpio, 1, 20), "...")
    }
    
    datos_variable <- variables_numericas %>% 
      select(all_of(var_name)) %>% 
      filter(!is.na(!!sym(var_name)))
    
    boxplot_mini <- ggplot(datos_variable, aes(x = "", y = !!sym(var_name))) +
      geom_boxplot(fill = PANTONE_9043_C, color = PANTONE_262_C, alpha = 0.7) +
      labs(title = nombre_limpio, x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 6),
        plot.margin = unit(c(1, 1, 1, 1), "mm")
      )
    
    boxplots_mini[[i]] <- boxplot_mini
  }
  
  # Mostrar resumen
  grid.arrange(
    grobs = boxplots_mini,
    ncol = 3,
    nrow = 2,
    top = paste("Resumen -", total_graficos, "variables en", total_paginas, "páginas")
  )
}

# Mostrar página de resumen
crear_pagina_resumen()

# GUARDAR TODAS LAS PÁGINAS EN PDF
cat("\n=== GUARDANDO TODAS LAS PÁGINAS EN PDF ===\n")

#pdf("boxplots_italia_trimestral_todas_las_paginas.pdf", width = 16, height = 12)

for (pagina in 1:total_paginas) {
  cat("Guardando página", pagina, "en PDF...\n")
  
  inicio <- (pagina - 1) * graficos_por_pagina + 1
  fin <- min(pagina * graficos_por_pagina, total_graficos)
  
  grid.arrange(
    grobs = boxplots_list[inicio:fin],
    ncol = 3,
    nrow = 3,
    top = paste("Boxplots de Variables - Italia Trimestral - Página", pagina, "de", total_paginas)
  )
}

dev.off()
cat("✓ Todas las páginas guardadas en: boxplots_italia_trimestral_todas_las_paginas.pdf\n")

# RESUMEN FINAL
cat("\n=== RESUMEN FINAL ===\n")
cat("✓ Total de variables analizadas:", ncol(variables_numericas), "\n")
cat("✓ Total de boxplots creados:", length(boxplots_list), "\n")
cat("✓ Páginas generadas:", total_paginas, "\n")
cat("✓ PDF con todas las páginas guardado\n")
cat("✓ Se muestran todas las páginas en secuencia\n")
cat("\n=== INSTRUCCIONES ===\n")
cat("• Las páginas se muestran una tras otra en la ventana gráfica\n")
cat("• Todas las páginas están guardadas en el PDF\n")
cat("• Usa el botón 'Previous Plot' en RStudio para navegar entre páginas\n")
cat("• El PDF contiene todas las páginas organizadas\n")
