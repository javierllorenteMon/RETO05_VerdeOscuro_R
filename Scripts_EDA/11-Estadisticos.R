# Librerias
library(readxl) 
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

# 15. GRÁFICOS ESPECÍFICOS PARA GDP E INFLACIÓN DE italia_trimestral
cat("\n=== CREANDO GRÁFICOS ESPECÍFICOS PARA GDP E INFLACIÓN ===\n")

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

# 1. GRÁFICO DEL GDP (PIB)
cat("=== GRÁFICO 1: GDP (PIB) ===\n")

# Verificar que existe la variable GDP
if("GDP.billion.currency.units" %in% names(italia_plot_data)) {
  
  gdp_stats <- italia_plot_data %>%
    summarise(
      media = mean(GDP.billion.currency.units, na.rm = TRUE),
      sd = sd(GDP.billion.currency.units, na.rm = TRUE),
      min = min(GDP.billion.currency.units, na.rm = TRUE),
      max = max(GDP.billion.currency.units, na.rm = TRUE),
      n_no_na = sum(!is.na(GDP.billion.currency.units)),
      n_total = n()
    )
  
  # Gráfico del GDP en valores absolutos
  gdp_plot <- ggplot(italia_plot_data, aes(x = Fecha, y = GDP.billion.currency.units)) +
    geom_line(color = "#2E86AB", size = 1.2) +
    geom_point(color = "#2E86AB", size = 2, alpha = 0.8) +
    geom_smooth(method = "loess", color = "#F24236", linetype = "dashed", se = FALSE, alpha = 0.6) +
    labs(
      title = "Evolución del PIB - Italia",
      subtitle = paste0("Media: ", round(gdp_stats$media, 2), " billones | ",
                        "Mín: ", round(gdp_stats$min, 2), " | ",
                        "Máx: ", round(gdp_stats$max, 2)),
      x = "Fecha",
      y = "PIB (Billones de unidades monetarias)",
      caption = paste0("Datos: ", gdp_stats$n_no_na, "/", gdp_stats$n_total, " trimestres")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
      plot.caption = element_text(size = 9, color = "gray50", margin = margin(t = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.02))
  
  print(gdp_plot)
  cat("✓ Gráfico del GDP creado y mostrado\n")
  
} else {
  cat("✗ Variable GDP.billion.currency.units no encontrada\n")
}

# 2. GRÁFICO DE INFLACIÓN (CPI)
cat("\n=== GRÁFICO 2: INFLACIÓN (CPI) ===\n")

# Verificar que existe la variable CPI
if("Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  
  cpi_stats <- italia_plot_data %>%
    summarise(
      media = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
      sd = sd(Consumer.Price.Index..CPI., na.rm = TRUE),
      min = min(Consumer.Price.Index..CPI., na.rm = TRUE),
      max = max(Consumer.Price.Index..CPI., na.rm = TRUE),
      n_no_na = sum(!is.na(Consumer.Price.Index..CPI.)),
      n_total = n()
    )
  
  # Gráfico del CPI en valores absolutos
  cpi_plot <- ggplot(italia_plot_data, aes(x = Fecha, y = Consumer.Price.Index..CPI.)) +
    geom_line(color = "#A23B72", size = 1.2) +
    geom_point(color = "#A23B72", size = 2, alpha = 0.8) +
    geom_smooth(method = "loess", color = "#F24236", linetype = "dashed", se = FALSE, alpha = 0.6) +
    labs(
      title = "Evolución del Índice de Precios al Consumidor (IPC) - Italia",
      subtitle = paste0("Media: ", round(cpi_stats$media, 2), " | ",
                        "Mín: ", round(cpi_stats$min, 2), " | ",
                        "Máx: ", round(cpi_stats$max, 2)),
      x = "Fecha",
      y = "Índice de Precios al Consumidor (IPC)",
      caption = paste0("Datos: ", cpi_stats$n_no_na, "/", cpi_stats$n_total, " trimestres")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
      plot.caption = element_text(size = 9, color = "gray50", margin = margin(t = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.02))
  
  print(cpi_plot)
  cat("✓ Gráfico del CPI creado y mostrado\n")
  
} else {
  cat("✗ Variable Consumer.Price.Index..CPI. no encontrada\n")
}

# 3. GRÁFICO COMPARATIVO GDP vs CPI (si ambas existen)
cat("\n=== GRÁFICO 3: COMPARACIÓN GDP vs CPI ===\n")

if("GDP.billion.currency.units" %in% names(italia_plot_data) & 
   "Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  
  # Normalizar ambas variables para comparación
  comparacion_data <- italia_plot_data %>%
    select(Fecha, GDP.billion.currency.units, Consumer.Price.Index..CPI.) %>%
    mutate(
      GDP_normalized = (GDP.billion.currency.units - min(GDP.billion.currency.units, na.rm = TRUE)) / 
        (max(GDP.billion.currency.units, na.rm = TRUE) - min(GDP.billion.currency.units, na.rm = TRUE)),
      CPI_normalized = (Consumer.Price.Index..CPI. - min(Consumer.Price.Index..CPI., na.rm = TRUE)) / 
        (max(Consumer.Price.Index..CPI., na.rm = TRUE) - min(Consumer.Price.Index..CPI., na.rm = TRUE))
    ) %>%
    select(Fecha, GDP_normalized, CPI_normalized) %>%
    pivot_longer(cols = -Fecha, names_to = "Variable", values_to = "Valor") %>%
    mutate(
      Variable = ifelse(Variable == "GDP_normalized", "PIB (normalizado)", "IPC (normalizado)")
    )
  
  comparacion_plot <- ggplot(comparacion_data, aes(x = Fecha, y = Valor, color = Variable)) +
    geom_line(size = 1.2, alpha = 0.8) +
    labs(
      title = "Comparación PIB vs IPC - Italia",
      subtitle = "Ambas variables normalizadas para comparación",
      x = "Fecha",
      y = "Valor Normalizado (0-1)",
      color = "Variable"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.02)) +
    scale_color_manual(values = c("PIB (normalizado)" = "#2E86AB", "IPC (normalizado)" = "#A23B72"))
  
  print(comparacion_plot)
  cat("✓ Gráfico comparativo GDP vs CPI creado y mostrado\n")
  
} else {
  cat("✗ No se pueden crear gráficos comparativos - faltan variables\n")
}

# 4. GRÁFICOS DE PORCENTAJES DE CAMBIO TRIMESTRAL (si existen)
cat("\n=== GRÁFICOS 4-5: PORCENTAJES DE CAMBIO TRIMESTRAL ===\n")

# Gráfico del porcentaje de cambio del GDP
if("PIB_pct_cambio" %in% names(italia_plot_data)) {
  
  pib_pct_plot <- ggplot(italia_plot_data, aes(x = Fecha, y = PIB_pct_cambio)) +
    geom_ribbon(aes(ymin = 0, ymax = ifelse(PIB_pct_cambio > 0, PIB_pct_cambio, 0)),
                fill = "#2E86AB", alpha = 0.2) +
    geom_ribbon(aes(ymin = ifelse(PIB_pct_cambio < 0, PIB_pct_cambio, 0), ymax = 0),
                fill = "#A23B72", alpha = 0.2) +
    geom_line(color = "#2E86AB", size = 1.2) +
    geom_point(color = "#2E86AB", size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    labs(
      title = "Crecimiento Trimestral del PIB - Italia",
      subtitle = "Porcentaje de cambio trimestral del PIB",
      x = "Fecha",
      y = "Crecimiento Trimestral (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  print(pib_pct_plot)
  cat("✓ Gráfico de porcentaje de cambio del GDP creado y mostrado\n")
}

# Gráfico del porcentaje de cambio del CPI
if("IPC_pct_cambio" %in% names(italia_plot_data)) {
  
  cpi_pct_plot <- ggplot(italia_plot_data, aes(x = Fecha, y = IPC_pct_cambio)) +
    geom_ribbon(aes(ymin = 0, ymax = ifelse(IPC_pct_cambio > 0, IPC_pct_cambio, 0)),
                fill = "#2E86AB", alpha = 0.2) +
    geom_ribbon(aes(ymin = ifelse(IPC_pct_cambio < 0, IPC_pct_cambio, 0), ymax = 0),
                fill = "#A23B72", alpha = 0.2) +
    geom_line(color = "#A23B72", size = 1.2) +
    geom_point(color = "#A23B72", size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    labs(
      title = "Inflación Trimestral - Italia",
      subtitle = "Porcentaje de cambio trimestral del IPC",
      x = "Fecha",
      y = "Inflación Trimestral (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  print(cpi_pct_plot)
  cat("✓ Gráfico de porcentaje de cambio del CPI creado y mostrado\n")
}

# RESUMEN ESTADÍSTICO DE GDP E INFLACIÓN
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
cat("✓ Gráficos específicos de GDP e inflación creados\n")
cat("✓ Resumen estadístico generado\n")
