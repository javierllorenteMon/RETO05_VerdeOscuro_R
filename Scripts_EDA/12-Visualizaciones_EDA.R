################################################################################
# NUEVO BLOQUE: GRÁFICOS COMPARATIVOS POR GRUPOS CLAVE (ITALIA TRIMESTRAL)
################################################################################

cat("\n=== CREANDO GRÁFICOS COMPARATIVOS POR GRUPOS CLAVE ===\n")

# Asegurar que 'italia_plot_data' existe
if (!exists("italia_plot_data")) {
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
}

# Paleta Pantone
PANTONE_220_C <- "#A50050"
PANTONE_376_C <- "#84BD00"
PANTONE_262_C <- "#51284F"
PANTONE_9043_C <- "#EAE7E0"
PANTONE_7545_C <- "#3B4B5A"
PANTONE_1375_C <- "#FFA300"

# Función genérica para crear comparaciones dobles
crear_comparativo <- function(df, var1, var2, nombre1, nombre2, color1, color2, titulo, eje_derecho = TRUE) {
  if(!(var1 %in% names(df)) | !(var2 %in% names(df))) {
    cat("✗ No se pueden graficar:", nombre1, "y", nombre2, "(faltan variables)\n")
    return(NULL)
  }
  
  plot_ly(df, x = ~Fecha) %>%
    add_trace(y = as.formula(paste0("~", var1)),
              type = 'scatter', mode = 'lines+markers',
              line = list(color = color1, width = 3),
              marker = list(color = color1, size = 6, opacity = 0.7),
              name = nombre1,
              yaxis = 'y1') %>%
    add_trace(y = as.formula(paste0("~", var2)),
              type = 'scatter', mode = 'lines+markers',
              line = list(color = color2, width = 3, dash = 'dash'),
              marker = list(color = color2, size = 6, opacity = 0.7),
              name = nombre2,
              yaxis = if(eje_derecho) 'y2' else 'y1') %>%
    layout(
      title = list(text = paste0("<b>", titulo, "</b>"), x = 0.5),
      xaxis = list(title = "Fecha", tickformat = "%Y", tickangle = -45, gridcolor = PANTONE_9043_C),
      yaxis = list(title = nombre1, color = color1),
      yaxis2 = if(eje_derecho) list(title = nombre2, overlaying = "y", side = "right", color = color2) else NULL,
      legend = list(orientation = "h", x = 0.3, y = -0.2),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white'
    )
}

# Función para gráficos lineales simples
crear_grafico_lineal <- function(df, variable, color, titulo, eje_y = "Valor") {
  if(!(variable %in% names(df))) {
    cat("✗ No se puede graficar:", variable, "(variable no encontrada)\n")
    return(NULL)
  }
  
  plot_ly(df, x = ~Fecha, y = as.formula(paste0("~", variable)),
          type = 'scatter', mode = 'lines',
          line = list(color = color, width = 3),
          name = variable) %>%
    layout(
      title = list(text = paste0("<b>", titulo, "</b>"), x = 0.5),
      xaxis = list(title = "Fecha", tickformat = "%Y", tickangle = -45, gridcolor = PANTONE_9043_C),
      yaxis = list(title = eje_y),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      showlegend = FALSE
    )
}

# Verificar variables disponibles
cat("\n=== VERIFICANDO VARIABLES DISPONIBLES ===\n")
variables_disponibles <- names(italia_plot_data)
cat("Variables en el dataset:", paste(variables_disponibles, collapse = ", "), "\n")

# Crear y mostrar gráficos individualmente
cat("\n--- EJECUTAR CADA LÍNEA INDIVIDUALMENTE PARA VER CADA GRÁFICO ---\n")

### 1. PIB ###
cat("\n# 1. GRÁFICO PIB - Ejecutar esta línea:\n")
if ("GDP.billion.currency.units" %in% variables_disponibles) {
  gdp_plot <- plot_ly(italia_plot_data, x = ~Fecha, y = ~GDP.billion.currency.units,
                      type = 'scatter', mode = 'lines+markers',
                      line = list(color = PANTONE_262_C, width = 3),
                      marker = list(color = PANTONE_262_C, size = 6)) %>%
    layout(title = "<b>PIB - Italia</b>", yaxis = list(title = "Billones de unidades monetarias"))
  print(gdp_plot)
} else {
  cat("✗ Variable GDP.billion.currency.units no encontrada\n")
}

### 2. Inflación ###
cat("\n# 2. GRÁFICO INFLACIÓN - Ejecutar esta línea:\n")
if ("Consumer.Price.Index..CPI." %in% variables_disponibles) {
  inflacion_plot <- plot_ly(italia_plot_data, x = ~Fecha, y = ~Consumer.Price.Index..CPI.,
                            type = 'scatter', mode = 'lines+markers',
                            line = list(color = PANTONE_220_C, width = 3),
                            marker = list(color = PANTONE_220_C, size = 6)) %>%
    layout(title = "<b>Índice de Precios al Consumidor (Inflación)</b>", yaxis = list(title = "Índice"))
  print(inflacion_plot)
} else {
  cat("✗ Variable Consumer.Price.Index..CPI. no encontrada\n")
}

### 3. Empleo y Desempleo ###
cat("\n# 3. GRÁFICO EMPLEO Y DESEMPLEO - Ejecutar esta línea:\n")
if ("EMPLEO" %in% variables_disponibles & "DESEMPLEO" %in% variables_disponibles) {
  empleo_plot <- crear_comparativo(italia_plot_data, "EMPLEO", "DESEMPLEO",
                                   "Empleo (%)", "Desempleo (%)",
                                   PANTONE_376_C, PANTONE_220_C,
                                   "Empleo y Desempleo - Italia")
  if(!is.null(empleo_plot)) print(empleo_plot)
} else {
  cat("✗ Variables EMPLEO y/o DESEMPLEO no encontradas\n")
}

### 4. PRODUCTIVIDAD LABORAL - GRÁFICO LINEAL ###
cat("\n# 4. GRÁFICO PRODUCTIVIDAD LABORAL (LINEAL) - Ejecutar esta línea:\n")
if ("Productividad_laboral" %in% variables_disponibles) {
  productividad_plot <- crear_grafico_lineal(italia_plot_data, 
                                             "Productividad_laboral",
                                             PANTONE_7545_C,
                                             "Productividad Laboral - Italia",
                                             "Índice de Productividad")
  if(!is.null(productividad_plot)) print(productividad_plot)
} else {
  cat("✗ Variable Productividad_laboral no encontrada\n")
}

### 5. Ingresos y Gastos del Gobierno ###
cat("\n# 5. GRÁFICO INGRESOS VS GASTOS GOBIERNO - Ejecutar esta línea:\n")
gov_vars <- variables_disponibles[grepl("gov|government|gasto|ingreso|revenue|expenditure|deficit|surplus", 
                                        variables_disponibles, ignore.case = TRUE)]

if (length(gov_vars) >= 2) {
  gov_plot <- crear_comparativo(italia_plot_data,
                                gov_vars[1], gov_vars[2],
                                gov_vars[1], gov_vars[2],
                                PANTONE_376_C, PANTONE_220_C,
                                paste("Gobierno:", gov_vars[1], "vs", gov_vars[2]))
  if(!is.null(gov_plot)) print(gov_plot)
} else if ("Deficit_Surplus" %in% variables_disponibles) {
  gov_plot <- plot_ly(italia_plot_data, x = ~Fecha, y = ~Deficit_Surplus,
                      type = 'scatter', mode = 'lines+markers',
                      line = list(color = PANTONE_262_C, width = 3),
                      marker = list(color = PANTONE_262_C, size = 6)) %>%
    layout(title = "<b>Déficit/Superávit del Gobierno</b>", 
           yaxis = list(title = "Millones de unidades monetarias"))
  print(gov_plot)
} else {
  cat("✗ No se encontraron variables de gobierno suficientes. Variables disponibles:", paste(gov_vars, collapse = ", "), "\n")
}

### 6. DÉFICIT/SUPERÁVIT TOTAL VS % PIB - GRÁFICO COMPARATIVO ###
cat("\n# 6. GRÁFICO DÉFICIT/SUPERÁVIT TOTAL VS % PIB - Ejecutar esta línea:\n")
if ("Deficit_Surplus" %in% variables_disponibles & "Deficit_Surplus_Pct_PIB" %in% variables_disponibles) {
  deficit_plot <- crear_comparativo(italia_plot_data,
                                    "Deficit_Surplus", "Deficit_Surplus_Pct_PIB",
                                    "Déficit / Superávit (Total)", "Déficit / Superávit (% PIB)",
                                    PANTONE_262_C, PANTONE_1375_C,
                                    "Déficit / Superávit Total vs % PIB")
  if(!is.null(deficit_plot)) print(deficit_plot)
} else if ("Deficit_Surplus" %in% variables_disponibles) {
  deficit_plot <- plot_ly(italia_plot_data, x = ~Fecha, y = ~Deficit_Surplus,
                          type = 'scatter', mode = 'lines+markers',
                          line = list(color = PANTONE_262_C, width = 3),
                          marker = list(color = PANTONE_262_C, size = 6)) %>%
    layout(title = "<b>Déficit/Superávit del Gobierno</b>", 
           yaxis = list(title = "Millones de unidades monetarias"))
  print(deficit_plot)
} else {
  cat("✗ Variables de déficit/superávit no encontradas\n")
}

### 7. Exportaciones e Importaciones ###
cat("\n# 7. GRÁFICO EXPORTACIONES E IMPORTACIONES - Ejecutar esta línea:\n")
if ("exportaciones" %in% variables_disponibles & "importaciones" %in% variables_disponibles) {
  comercio_plot <- crear_comparativo(italia_plot_data,
                                     "exportaciones", "importaciones",
                                     "Exportaciones", "Importaciones",
                                     PANTONE_376_C, PANTONE_220_C,
                                     "Exportaciones e Importaciones - Italia")
  if(!is.null(comercio_plot)) print(comercio_plot)
} else {
  cat("✗ Variables exportaciones e importaciones no encontradas\n")
}

### 8. VARIACIÓN % TRIMESTRAL - EXPORTACIONES E IMPORTACIONES - GRÁFICO COMPARATIVO ###
cat("\n# 8. GRÁFICO VARIACIÓN % TRIMESTRAL - EXPORTACIONES E IMPORTACIONES - Ejecutar esta línea:\n")
if ("Exportaciones_pct_cambio" %in% variables_disponibles & "Importaciones_pct_cambio" %in% variables_disponibles) {
  comercio_pct_plot <- crear_comparativo(italia_plot_data,
                                         "Exportaciones_pct_cambio", "Importaciones_pct_cambio",
                                         "Exportaciones (%)", "Importaciones (%)",
                                         PANTONE_376_C, PANTONE_220_C,
                                         "Variación % Trimestral - Exportaciones e Importaciones")
  if(!is.null(comercio_pct_plot)) print(comercio_pct_plot)
} else {
  cat("✗ Variables de variación porcentual de comercio no encontradas\n")
}

### 9. TASA DE CRECIMIENTO DE PRECIOS DE VIVIENDA - GRÁFICO LINEAL ###
cat("\n# 9. GRÁFICO TASA DE CRECIMIENTO DE PRECIOS DE VIVIENDA (LINEAL) - Ejecutar esta línea:\n")
if ("Tasa_crecimiento_precios_vivienda" %in% variables_disponibles) {
  vivienda_plot <- crear_grafico_lineal(italia_plot_data, 
                                        "Tasa_crecimiento_precios_vivienda",
                                        PANTONE_7545_C,
                                        "Tasa de Crecimiento de Precios de Vivienda",
                                        "% Trimestral")
  if(!is.null(vivienda_plot)) print(vivienda_plot)
} else {
  cat("✗ Variable Tasa_crecimiento_precios_vivienda no encontrada\n")
}

### 10. ÍNDICE DE CONFIANZA DEL CONSUMIDOR (TRIMESTRAL) - GRÁFICO LINEAL ###
cat("\n# 10. GRÁFICO ÍNDICE DE CONFIANZA DEL CONSUMIDOR (LINEAL) - Ejecutar esta línea:\n")
if ("Indice_confianza_trimestral" %in% variables_disponibles) {
  confianza_plot <- crear_grafico_lineal(italia_plot_data, 
                                         "Indice_confianza_trimestral",
                                         PANTONE_262_C,
                                         "Índice de Confianza del Consumidor (Trimestral)",
                                         "Índice")
  if(!is.null(confianza_plot)) print(confianza_plot)
} else {
  cat("✗ Variable Indice_confianza_trimestral no encontrada\n")
}

# Gráficos adicionales con variables disponibles
cat("\n=== GRÁFICOS ADICIONALES CON VARIABLES DISPONIBLES ===\n")

# Encontrar variables numéricas adicionales
numeric_vars <- variables_disponibles[sapply(italia_plot_data, is.numeric)]
numeric_vars <- setdiff(numeric_vars, c("Year")) # Excluir año

if (length(numeric_vars) > 0) {
  cat("Variables numéricas disponibles para gráficos adicionales:", paste(numeric_vars, collapse = ", "), "\n")
  
  # Crear gráficos simples para variables numéricas no utilizadas
  vars_utilizadas <- c("GDP.billion.currency.units", "Consumer.Price.Index..CPI.", "EMPLEO", "DESEMPLEO",
                       "Productividad_laboral", "Deficit_Surplus", "Deficit_Surplus_Pct_PIB",
                       "exportaciones", "importaciones", "Exportaciones_pct_cambio", "Importaciones_pct_cambio",
                       "Tasa_crecimiento_precios_vivienda", "Indice_confianza_trimestral")
  
  vars_no_utilizadas <- setdiff(numeric_vars, vars_utilizadas)
  
  if (length(vars_no_utilizadas) > 0) {
    cat("\n--- GRÁFICOS DE VARIABLES ADICIONALES ---\n")
    for (i in seq_along(vars_no_utilizadas)) {
      if (i <= 3) { # Mostrar máximo 3 gráficos adicionales
        cat("\n# GRÁFICO ADICIONAL", i, "-", vars_no_utilizadas[i], "- Ejecutar esta línea:\n")
        plot_adicional <- crear_grafico_lineal(italia_plot_data,
                                               vars_no_utilizadas[i],
                                               PANTONE_7545_C,
                                               paste(vars_no_utilizadas[i], "- Italia"),
                                               vars_no_utilizadas[i])
        if(!is.null(plot_adicional)) print(plot_adicional)
      }
    }
  }
}

cat("\n✓ Proceso de generación de gráficos completado\n")
cat("RESUMEN DE MODIFICACIONES REALIZADAS:\n")
cat("• Productividad Laboral: Cambiado a gráfico lineal\n")
cat("• Déficit/Superávit: Mantenido como comparativo (Total vs % PIB)\n")
cat("• Variación % Comercio: Mantenido como comparativo\n")
cat("• Tasa Crecimiento Vivienda: Cambiado a gráfico lineal\n")
cat("• Índice Confianza: Cambiado a gráfico lineal\n")
cat("\nCopia y pega cada línea marcada con 'Ejecutar esta línea:' en la consola de R\n")

################################################################################
# FIN DEL BLOQUE DE GRÁFICOS COMPARATIVOS
################################################################################