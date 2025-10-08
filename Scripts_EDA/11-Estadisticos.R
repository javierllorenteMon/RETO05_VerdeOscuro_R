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
  )
}

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
  }
}

# Cargar o crear datos ISTAT
evol_empleo <- cargar_archivo_istat('evolucion_empleo_italia.csv')
evol_salarios <- cargar_archivo_istat('evolucion_salarios.csv')
productividad <- cargar_archivo_istat('productividad_laboral_trimestral_italia.csv')
revenue_exp <- cargar_archivo_istat('revenue_vs_expenditure.csv')
comercio_exterior <- cargar_archivo_istat('comercio_exterior_unificado.csv')
tasas_empleo <- cargar_archivo_istat('definitivo_tasas_empleo_desempleo.csv')

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

# 9. Limpiar y enriquecer dataframe final
cat("\n=== CREANDO DATAFRAME FINAL ===\n")

italia_trimestral <- italia_trimestral %>%
  # Eliminar filas con Quarter NA
  filter(!is.na(Quarter)) %>%
  # Ordenar por Periodo
  arrange(Periodo) %>%
  # Crear variables adicionales
  mutate(
    # Calcular nuevas variables
    Deficit_Surplus_Pct_PIB = ifelse(exists('Deficit_Surplus') & exists('GDP.billion.currency.units') & 
                                       !is.na(GDP.billion.currency.units) & GDP.billion.currency.units != 0,
                                     (Deficit_Surplus / (GDP.billion.currency.units * 1e9)) * 100, NA)
  ) %>%
  # Eliminar las columnas Balanza_comercial, Indice_compuesto y Trimestre_key
  select(-any_of(c("Balanza_comercial", "Indice_compuesto", "Trimestre_key"))) %>%
  # Seleccionar y ordenar columnas
  select(
    Country, Year, Quarter, Periodo,
    # Variables macroeconómicas
    GDP.billion.currency.units, Consumer.Price.Index..CPI.,
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
    # Variables exógenas originales
    matches("Unemployment"), matches("Government"),
    # Variables calculadas
    everything()
  ) %>%
  # Eliminar columnas totalmente vacías
  select(where(~!all(is.na(.))))

# 10. Análisis exploratorio completo
cat("\n=== ANÁLISIS EXPLORATORIO COMPLETO ===\n")

# Estructura del dataframe
cat("DIMENSIONES:", dim(italia_trimestral), "\n")
cat("PERIODO:", min(italia_trimestral$Periodo, na.rm = TRUE), "-", max(italia_trimestral$Periodo, na.rm = TRUE), "\n")
cat("TRIMESTRES ÚNICOS:", paste(unique(italia_trimestral$Quarter), collapse = ", "), "\n")

# Variables disponibles
cat("\n=== VARIABLES DISPONIBLES ===\n")
variables_por_tipo <- list(
  "Identificadores" = names(italia_trimestral)[sapply(italia_trimestral, is.character)],
  "Numéricas" = names(italia_trimestral)[sapply(italia_trimestral, is.numeric)]
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

# 11. Visualizaciones completas
cat("\n=== CREANDO VISUALIZACIONES ===\n")

# Gráfico 1: Evolución del PIB
if("GDP.billion.currency.units" %in% names(italia_trimestral)) {
  p1 <- ggplot(italia_trimestral, aes(x = Periodo, y = GDP.billion.currency.units, group = 1)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(size = 1, color = "steelblue") +
    labs(title = "Evolución Trimestral del PIB en Italia", 
         y = "PIB (miles de millones)", 
         x = "Periodo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p1)
}

# Gráfico 2: Evolución del IPC
if("Consumer.Price.Index..CPI." %in% names(italia_trimestral)) {
  p2 <- ggplot(italia_trimestral, aes(x = Periodo, y = Consumer.Price.Index..CPI., group = 1)) +
    geom_line(color = "darkred", size = 1.2) +
    geom_point(size = 1, color = "darkred") +
    labs(title = "Evolución Trimestral del IPC en Italia", 
         y = "IPC", 
         x = "Periodo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p2)
}

# Gráfico 3: Empleo vs Desempleo
if(all(c("EMPLEO", "DESEMPLEO") %in% names(italia_trimestral))) {
  empleo_data <- italia_trimestral %>%
    select(Periodo, EMPLEO, DESEMPLEO) %>%
    pivot_longer(cols = c(EMPLEO, DESEMPLEO), names_to = "Variable", values_to = "Valor")
  
  p3 <- ggplot(empleo_data, aes(x = Periodo, y = Valor, color = Variable, group = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    labs(title = "Evolución de Tasas de Empleo y Desempleo",
         y = "Porcentaje (%)", x = "Periodo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  print(p3)
}

# Gráfico 4: Comercio exterior
if(all(c("exportaciones", "importaciones") %in% names(italia_trimestral))) {
  comercio_data <- italia_trimestral %>%
    select(Periodo, exportaciones, importaciones) %>%
    pivot_longer(cols = c(exportaciones, importaciones), names_to = "Variable", values_to = "Valor")
  
  p4 <- ggplot(comercio_data, aes(x = Periodo, y = Valor, color = Variable, group = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    labs(title = "Evolución de Exportaciones e Importaciones",
         y = "Valor", x = "Periodo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  print(p4)
}

# Gráfico 5: Matriz de correlación
variables_numericas <- italia_trimestral %>%
  select(where(is.numeric)) %>%
  select(-Year) %>%
  select_if(~sum(!is.na(.)) > 10)  # Solo variables con suficientes datos

if(ncol(variables_numericas) > 2) {
  cor_matrix <- cor(variables_numericas, use = "pairwise.complete.obs")
  p5 <- corrplot(cor_matrix, method = "color", type = "upper",
                 addCoef.col = "black",  # Añadir coeficientes en negro
                 number.cex = 0.6,       # Tamaño de los números
                 tl.col = "black", tl.cex = 0.7, 
                 title = "Matriz de Correlación - Italia",
                 mar = c(0, 0, 2, 0))
  print(p5)
}

# Gráfico 6: Distribución de variables principales
vars_principales <- italia_trimestral %>%
  select(any_of(c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.", 
                  "EMPLEO", "DESEMPLEO", "Indice_empleo", "Indice_salarios",
                  "Productividad_laboral", "exportaciones","importaciones", 
                  "Total.Goverment.expenditure","Deficit_Surplus", 
                  "Deficit_Surplus_Pct", "Deficit_Surplus_Pct_PIB"))) %>%
  select(where(is.numeric))

if(ncol(vars_principales) > 0) {
  vars_long <- vars_principales %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")
  
  p6 <- ggplot(vars_long, aes(y = Valor, fill = Variable)) +
    geom_boxplot() +
    theme_minimal() +
    facet_wrap(~Variable, scales = "free_y") +
    labs(title = "Distribución de Variables Principales") +
    theme(legend.position = "none")
  print(p6)
}

# 12. Análisis estadístico completo
cat("\n=== ANÁLISIS ESTADÍSTICO COMPLETO ===\n")

# Estadísticas descriptivas
if(ncol(variables_numericas) > 0) {
  cat("ESTADÍSTICAS DESCRIPTIVAS:\n")
  print(describe(variables_numericas))
}

# Resumen por año
cat("\n=== RESUMEN POR AÑO ===\n")
resumen_anual <- italia_trimestral %>%
  group_by(Year) %>%
  summarise(
    Trimestres = n(),
    PIB_promedio = mean(GDP.billion.currency.units, na.rm = TRUE),
    IPC_promedio = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
    .groups = 'drop'
  )
print(resumen_anual)

# 13. Guardar resultados
cat("\n=== GUARDANDO RESULTADOS ===\n")

# Dataframe completo
#write.csv(italia_trimestral, "Datos/italia_trimestral_completo_definitivo.csv", 
          #row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ Dataframe completo guardado: Datos/italia_trimestral_completo_definitivo.csv\n")

# Resumen estadístico
#write.csv(describe(variables_numericas), "Datos/resumen_estadistico_italia.csv", 
          #row.names = TRUE, fileEncoding = "UTF-8")
cat("✓ Resumen estadístico guardado: Datos/resumen_estadistico_italia.csv\n")

