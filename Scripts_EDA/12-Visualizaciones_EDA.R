# =============================================================================
# GRÁFICOS POR ÁREAS ECONÓMICAS CON PLOTLY Y PALETA PANTONE - VERSIÓN FINAL
# =============================================================================

# Librerías requeridas
library(plotly)
library(dplyr)
library(lubridate)
library(htmlwidgets)
library(webshot2)

# Definir paleta de colores Pantone del archivo proporcionado
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
    Quarter = substr(Periodo, 6, 7),
    # Crear etiquetas de trimestre para hover
    Trimestre_Label = case_when(
      Quarter == "Q1" ~ "1er Trimestre",
      Quarter == "Q2" ~ "2do Trimestre", 
      Quarter == "Q3" ~ "3er Trimestre",
      Quarter == "Q4" ~ "4to Trimestre"
    )
  ) %>%
  arrange(Fecha)

# Función para formatear porcentajes con signo
formatear_porcentaje <- function(valor) {
  ifelse(is.na(valor), NA,
         ifelse(valor >= 0, 
                sprintf("+%.2f%%", valor), 
                sprintf("%.2f%%", valor)))
}

# Aplicar formato de porcentajes con signo a los datos
italia_plot_data <- italia_plot_data %>%
  mutate(
    PIB_pct_cambio_formatted = formatear_porcentaje(PIB_pct_cambio),
    IPC_pct_cambio_formatted = formatear_porcentaje(IPC_pct_cambio),
    Precios_vivienda_pct_cambio_formatted = formatear_porcentaje(Precios_vivienda_pct_cambio),
    Deficit_Surplus_Pct_PIB_formatted = formatear_porcentaje(Deficit_Surplus_Pct_PIB),
    Salarios_pct_cambio_formatted = formatear_porcentaje(Salarios_pct_cambio),
    Exportaciones_pct_cambio_formatted = formatear_porcentaje(Exportaciones_pct_cambio),
    Importaciones_pct_cambio_formatted = formatear_porcentaje(Importaciones_pct_cambio)
  )

# Función para crear formato de eje X con solo años
crear_eje_x_anual <- function(datos) {
  años_unicos <- unique(datos$Year)
  fechas_referencia <- as.Date(paste0(años_unicos, "-01-01"))
  
  list(
    title = "Año",
    tickmode = "array",
    tickvals = fechas_referencia,
    ticktext = años_unicos,
    tickangle = -45,
    gridcolor = PANTONE_9043_C,
    titlefont = list(color = PANTONE_262_C),
    tickfont = list(color = PANTONE_262_C),
    showgrid = TRUE
  )
}

# Configuración común para todos los gráficos
config_comun <- list(
  displayModeBar = TRUE,
  displaylogo = FALSE,
  modeBarButtonsToRemove = c("lasso2d", "select2d", "toggleSpikelines"),
  toImageButtonOptions = list(
    format = "png",
    filename = "grafico_italia",
    height = 500,
    width = 800,
    scale = 2
  )
)

# =============================================================================
# 1. ÁREA: CRECIMIENTO ECONÓMICO - MODIFICADO (GDP.billion.currency.units)
# =============================================================================
cat("=== CREANDO GRÁFICOS DE CRECIMIENTO ECONÓMICO ===\n")

# Gráfico 1.1: PIB Nominal (GDP.billion.currency.units)
if("GDP.billion.currency.units" %in% names(italia_plot_data)) {
  
  datos_pib_nominal <- italia_plot_data %>% filter(!is.na(GDP.billion.currency.units))
  
  p1_pib_nominal <- plot_ly(datos_pib_nominal) %>%
    add_trace(x = ~Fecha, y = ~GDP.billion.currency.units, 
              type = 'scatter', mode = 'lines+markers',
              name = 'PIB Nominal',
              line = list(color = PANTONE_262_C, width = 3),
              marker = list(color = PANTONE_262_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>PIB Nominal:</b> %{y:.2f} billones<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>PIB Nominal - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_pib_nominal),
      yaxis = list(
        title = "PIB Nominal (Billones de unidades monetarias)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p1_pib_nominal)
  cat("✓ Gráfico del PIB Nominal creado\n")
}

# Gráfico 1.2: Crecimiento Trimestral del PIB Nominal (PIB_pct_cambio)
if("PIB_pct_cambio" %in% names(italia_plot_data)) {
  
  datos_crecimiento <- italia_plot_data %>% filter(!is.na(PIB_pct_cambio))
  
  p2_crecimiento_pib <- plot_ly(datos_crecimiento) %>%
    add_trace(x = ~Fecha, y = ~PIB_pct_cambio, 
              type = 'bar',
              name = 'Crecimiento Trimestral PIB',
              marker = list(
                color = ~ifelse(PIB_pct_cambio >= 0, PANTONE_376_C, PANTONE_220_C),
                opacity = 0.7
              ),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Crecimiento PIB:</b> %{text}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              text = ~PIB_pct_cambio_formatted,
              showlegend = FALSE,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    add_trace(x = ~Fecha, y = ~mean(PIB_pct_cambio, na.rm = TRUE),
              type = 'scatter', mode = 'lines',
              name = 'Media',
              line = list(color = PANTONE_262_C, width = 2, dash = 'dash'),
              hovertemplate = paste(
                "<b>Media crecimiento:</b> %{text}<br>",
                "<extra></extra>"
              ),
              text = ~formatear_porcentaje(mean(PIB_pct_cambio, na.rm = TRUE)),
              showlegend = TRUE) %>%
    layout(
      title = list(
        text = "<b>Crecimiento Trimestral del PIB Nominal - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_crecimiento),
      yaxis = list(
        title = "Crecimiento Trimestral (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C),
        showgrid = TRUE
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p2_crecimiento_pib)
  cat("✓ Gráfico de crecimiento del PIB Nominal creado (sin etiquetas en barras)\n")
}

# Gráfico 1.3: Deflactor del PIB - CORREGIDO (sin relleno)
if("Deflactor_PIB" %in% names(italia_plot_data)) {
  
  datos_deflactor <- italia_plot_data %>% filter(!is.na(Deflactor_PIB))
  
  p3_deflactor <- plot_ly(datos_deflactor) %>%
    add_trace(x = ~Fecha, y = ~Deflactor_PIB, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Deflactor PIB',
              line = list(color = PANTONE_220_C, width = 3),
              marker = list(color = PANTONE_220_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Deflactor:</b> %{y:.2f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Deflactor del PIB - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_220_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_deflactor),
      yaxis = list(
        title = "Deflactor (2015=100)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_220_C,
        font = list(color = PANTONE_220_C)
      )
    ) %>%
    config(config_comun)
  
  print(p3_deflactor)
  cat("✓ Gráfico del deflactor del PIB creado (sin relleno)\n")
}

# =============================================================================
# 2. ÁREA: INFLACIÓN
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DE INFLACIÓN ===\n")

# Gráfico 2.1: IPC
if("Consumer.Price.Index..CPI." %in% names(italia_plot_data)) {
  
  datos_ipc <- italia_plot_data %>% filter(!is.na(Consumer.Price.Index..CPI.))
  
  p4_inflacion <- plot_ly(datos_ipc) %>%
    add_trace(x = ~Fecha, y = ~Consumer.Price.Index..CPI., 
              type = 'scatter', mode = 'lines+markers',
              name = 'IPC',
              line = list(color = PANTONE_220_C, width = 3),
              marker = list(color = PANTONE_220_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>IPC:</b> %{y:.2f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Índice de Precios al Consumidor (IPC) - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_220_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_ipc),
      yaxis = list(
        title = "IPC",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_220_C,
        font = list(color = PANTONE_220_C)
      )
    ) %>%
    config(config_comun)
  
  print(p4_inflacion)
  cat("✓ Gráfico del IPC creado\n")
}

# Gráfico 2.2: Inflación Trimestral - CORREGIDO (colores intercambiados y sin etiquetas)
if("IPC_pct_cambio" %in% names(italia_plot_data)) {
  
  datos_inflacion <- italia_plot_data %>% filter(!is.na(IPC_pct_cambio))
  
  p5_inflacion_trimestral <- plot_ly(datos_inflacion) %>%
    add_trace(x = ~Fecha, y = ~IPC_pct_cambio, 
              type = 'bar',
              name = 'Inflación Trimestral',
              marker = list(
                # COLORES INTERCAMBIADOS: positivo en verde, negativo en rojo
                color = ~ifelse(IPC_pct_cambio >= 0, PANTONE_376_C, PANTONE_220_C),
                opacity = 0.7
              ),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Inflación Trimestral:</b> %{text}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              text = ~IPC_pct_cambio_formatted,
              showlegend = FALSE,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    add_trace(x = ~Fecha, y = ~mean(IPC_pct_cambio, na.rm = TRUE),
              type = 'scatter', mode = 'lines',
              name = 'Media',
              line = list(color = PANTONE_262_C, width = 2, dash = 'dash'),
              hovertemplate = paste(
                "<b>Media inflación:</b> %{text}<br>",
                "<extra></extra>"
              ),
              text = ~formatear_porcentaje(mean(IPC_pct_cambio, na.rm = TRUE)),
              showlegend = TRUE) %>%
    layout(
      title = list(
        text = "<b>Inflación Trimestral (%) - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_220_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_inflacion),
      yaxis = list(
        title = "Cambio Porcentual Trimestral (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_220_C,
        font = list(color = PANTONE_220_C)
      )
    ) %>%
    config(config_comun)
  
  print(p5_inflacion_trimestral)
  cat("✓ Gráfico de inflación trimestral creado (colores intercambiados y sin etiquetas)\n")
}

# =============================================================================
# 3. ÁREA: MERCADO LABORAL
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DEL MERCADO LABORAL ===\n")

# Gráfico 3.1: Empleo y Desempleo
if("EMPLEO" %in% names(italia_plot_data) & "DESEMPLEO" %in% names(italia_plot_data)) {
  
  datos_empleo <- italia_plot_data %>% filter(!is.na(EMPLEO) & !is.na(DESEMPLEO))
  
  p6_empleo <- plot_ly(datos_empleo) %>%
    add_trace(x = ~Fecha, y = ~EMPLEO, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Tasa de Empleo',
              line = list(color = PANTONE_376_C, width = 3),
              marker = list(color = PANTONE_376_C, size = 6),
              yaxis = 'y1',
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Tasa Empleo:</b> %{y:.1f}%<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    add_trace(x = ~Fecha, y = ~DESEMPLEO, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Tasa de Desempleo',
              line = list(color = PANTONE_220_C, width = 3),
              marker = list(color = PANTONE_220_C, size = 6),
              yaxis = 'y2',
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Tasa Desempleo:</b> %{y:.1f}%<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Tasas de Empleo y Desempleo - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_empleo),
      yaxis = list(
        title = "Tasa de Empleo (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_376_C),
        tickfont = list(color = PANTONE_376_C),
        side = 'left'
      ),
      yaxis2 = list(
        title = "Tasa de Desempleo (%)",
        overlaying = "y",
        side = "right",
        gridcolor = 'rgba(0,0,0,0)',
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p6_empleo)
  cat("✓ Gráfico de empleo/desempleo creado\n")
}

# Gráfico 3.2: Evolución de Salarios
if("Indice_salarios" %in% names(italia_plot_data)) {
  
  datos_salarios <- italia_plot_data %>% filter(!is.na(Indice_salarios))
  
  p7_salarios <- plot_ly(datos_salarios) %>%
    add_trace(x = ~Fecha, y = ~Indice_salarios, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Índice de Salarios',
              line = list(color = PANTONE_376_C, width = 3),
              marker = list(color = PANTONE_376_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Índice Salarios:</b> %{y:.1f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Evolución de los Salarios - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_376_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_salarios),
      yaxis = list(
        title = "Índice de Salarios",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_376_C),
        tickfont = list(color = PANTONE_376_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_376_C,
        font = list(color = PANTONE_376_C)
      )
    ) %>%
    config(config_comun)
  
  print(p7_salarios)
  cat("✓ Gráfico de salarios creado\n")
}

# Gráfico 3.3: Productividad Laboral - CORREGIDO (sin relleno)
if("Productividad_laboral" %in% names(italia_plot_data)) {
  
  datos_productividad <- italia_plot_data %>% filter(!is.na(Productividad_laboral))
  
  p8_productividad <- plot_ly(datos_productividad) %>%
    add_trace(x = ~Fecha, y = ~Productividad_laboral, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Productividad',
              line = list(color = PANTONE_262_C, width = 3),
              marker = list(color = PANTONE_262_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Productividad:</b> %{y:.2f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Productividad Laboral - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_productividad),
      yaxis = list(
        title = "Índice de Productividad",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p8_productividad)
  cat("✓ Gráfico de productividad laboral creado (sin relleno)\n")
}

# =============================================================================
# 4. ÁREA: COMERCIO EXTERIOR
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DE COMERCIO EXTERIOR ===\n")

# Gráfico 4.1: Exportaciones vs Importaciones
if("exportaciones" %in% names(italia_plot_data) & "importaciones" %in% names(italia_plot_data)) {
  
  datos_comercio <- italia_plot_data %>% 
    filter(!is.na(exportaciones) & !is.na(importaciones))
  
  p9_comercio <- plot_ly(datos_comercio) %>%
    add_trace(x = ~Fecha, y = ~exportaciones, 
              type = 'scatter', mode = 'lines',
              name = 'Exportaciones',
              line = list(color = PANTONE_376_C, width = 3),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Exportaciones:</b> %{y:,.0f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    add_trace(x = ~Fecha, y = ~importaciones, 
              type = 'scatter', mode = 'lines',
              name = 'Importaciones',
              line = list(color = PANTONE_220_C, width = 3),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Importaciones:</b> %{y:,.0f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Exportaciones vs Importaciones - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_comercio),
      yaxis = list(
        title = "Valor (Millones)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p9_comercio)
  cat("✓ Gráfico de comercio exterior creado\n")
}

# Gráfico 4.2: Balanza Comercial - CORREGIDO (sin etiquetas en barras)
if("exportaciones" %in% names(italia_plot_data) & "importaciones" %in% names(italia_plot_data)) {
  
  datos_balanza <- italia_plot_data %>% 
    filter(!is.na(exportaciones) & !is.na(importaciones)) %>%
    mutate(Balanza = exportaciones - importaciones)
  
  p10_balanza <- plot_ly(datos_balanza) %>%
    add_trace(x = ~Fecha, y = ~Balanza, 
              type = 'bar',
              name = 'Balanza Comercial',
              marker = list(
                color = ~ifelse(Balanza >= 0, PANTONE_376_C, PANTONE_220_C),
                opacity = 0.7
              ),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Balanza:</b> %{y:,.0f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              showlegend = FALSE,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    layout(
      title = list(
        text = "<b>Balanza Comercial - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_balanza),
      yaxis = list(
        title = "Exportaciones - Importaciones",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p10_balanza)
  cat("✓ Gráfico de balanza comercial creado (sin etiquetas en barras)\n")
}

# =============================================================================
# 5. ÁREA: FINANZAS PÚBLICAS
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DE FINANZAS PÚBLICAS ===\n")

# Gráfico 5.1: Ingresos vs Gastos del Gobierno
if("Total.government.revenue" %in% names(italia_plot_data) & "Total.Government.expenditure" %in% names(italia_plot_data)) {
  
  datos_finanzas <- italia_plot_data %>% 
    filter(!is.na(Total.government.revenue) & !is.na(Total.Government.expenditure))
  
  p11_finanzas <- plot_ly(datos_finanzas) %>%
    add_trace(x = ~Fecha, y = ~Total.government.revenue, 
              type = 'scatter', mode = 'lines',
              name = 'Ingresos',
              line = list(color = PANTONE_376_C, width = 3),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Ingresos:</b> %{y:,.0f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    add_trace(x = ~Fecha, y = ~Total.Government.expenditure, 
              type = 'scatter', mode = 'lines',
              name = 'Gastos',
              line = list(color = PANTONE_220_C, width = 3),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Gastos:</b> %{y:,.0f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Ingresos vs Gastos del Gobierno - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_finanzas),
      yaxis = list(
        title = "Millones de Unidades Monetarias",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p11_finanzas)
  cat("✓ Gráfico de finanzas públicas creado\n")
}

# Gráfico 5.2: Déficit/Superávit como % del PIB - CORREGIDO (sin etiquetas en barras)
if("Deficit_Surplus_Pct_PIB" %in% names(italia_plot_data)) {
  
  datos_deficit <- italia_plot_data %>% filter(!is.na(Deficit_Surplus_Pct_PIB))
  
  # Crear datos separados para déficit y superávit
  datos_superavit <- datos_deficit %>% filter(Deficit_Surplus_Pct_PIB >= 0)
  datos_deficit_only <- datos_deficit %>% filter(Deficit_Surplus_Pct_PIB < 0)
  
  p12_deficit <- plot_ly() %>%
    # Superávit - Verde
    add_trace(data = datos_superavit,
              x = ~Fecha, y = ~Deficit_Surplus_Pct_PIB, 
              type = 'bar',
              name = 'Superávit',
              marker = list(color = PANTONE_376_C, opacity = 0.7),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Superávit:</b> %{text}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              text = ~Deficit_Surplus_Pct_PIB_formatted,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    # Déficit - Morado
    add_trace(data = datos_deficit_only,
              x = ~Fecha, y = ~Deficit_Surplus_Pct_PIB, 
              type = 'bar',
              name = 'Déficit',
              marker = list(color = PANTONE_262_C, opacity = 0.7),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Déficit:</b> %{text}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              text = ~Deficit_Surplus_Pct_PIB_formatted,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    add_trace(x = ~Fecha, y = ~0,
              type = 'scatter', mode = 'lines',
              name = 'Equilibrio',
              line = list(color = 'black', width = 1, dash = 'solid'),
              showlegend = FALSE) %>%
    add_trace(x = ~Fecha, y = ~-3,
              type = 'scatter', mode = 'lines',
              name = 'Límite Maastricht (-3%)',
              line = list(color = PANTONE_220_C, width = 2, dash = 'dash'),
              hovertemplate = paste(
                "<b>Límite Maastricht: -3%</b><br>",
                "<extra></extra>"
              ),
              showlegend = TRUE) %>%
    layout(
      title = list(
        text = "<b>Déficit/Superávit como % del PIB - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_deficit),
      yaxis = list(
        title = "Porcentaje del PIB (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      ),
      barmode = 'overlay'
    ) %>%
    config(config_comun)
  
  print(p12_deficit)
  cat("✓ Gráfico de déficit/superávit creado (sin etiquetas en barras)\n")
}

# =============================================================================
# 6. ÁREA: POLÍTICA MONETARIA
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DE POLÍTICA MONETARIA ===\n")

# Gráfico 6.1: Tipo de Interés a 10 años - CORREGIDO (sin relleno)
if("Tipo_interes_10y" %in% names(italia_plot_data)) {
  
  datos_interes <- italia_plot_data %>% filter(!is.na(Tipo_interes_10y))
  
  p13_interes <- plot_ly(datos_interes) %>%
    add_trace(x = ~Fecha, y = ~Tipo_interes_10y, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Tipo Interés 10 años',
              line = list(color = PANTONE_262_C, width = 3),
              marker = list(color = PANTONE_262_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Tipo Interés:</b> %{y:.2f}%<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Tipo de Interés a 10 Años - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_262_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_interes),
      yaxis = list(
        title = "Tipo de Interés (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_262_C),
        tickfont = list(color = PANTONE_262_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_262_C,
        font = list(color = PANTONE_262_C)
      )
    ) %>%
    config(config_comun)
  
  print(p13_interes)
  cat("✓ Gráfico de tipos de interés creado (sin relleno)\n")
}

# =============================================================================
# 7. ÁREA: CONFIANZA E INMOBILIARIO
# =============================================================================
cat("\n=== CREANDO GRÁFICOS DE CONFIANZA E INMOBILIARIO ===\n")

# Gráfico 7.1: Índice de Confianza del Consumidor
if("Indice_confianza_compuesto" %in% names(italia_plot_data)) {
  
  datos_confianza <- italia_plot_data %>% filter(!is.na(Indice_confianza_compuesto))
  
  p14_confianza <- plot_ly(datos_confianza) %>%
    add_trace(x = ~Fecha, y = ~Indice_confianza_compuesto, 
              type = 'scatter', mode = 'lines+markers',
              name = 'Confianza Consumidor',
              line = list(color = PANTONE_376_C, width = 3),
              marker = list(color = PANTONE_376_C, size = 6),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Confianza:</b> %{y:.1f}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label) %>%
    layout(
      title = list(
        text = "<b>Índice de Confianza del Consumidor - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_376_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_confianza),
      yaxis = list(
        title = "Índice de Confianza",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_376_C),
        tickfont = list(color = PANTONE_376_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_376_C,
        font = list(color = PANTONE_376_C)
      )
    ) %>%
    config(config_comun)
  
  print(p14_confianza)
  cat("✓ Gráfico de confianza del consumidor creado\n")
}

# Gráfico 7.2: Crecimiento de Precios de Vivienda - CORREGIDO (sin etiquetas en barras)
if("Precios_vivienda_pct_cambio" %in% names(italia_plot_data)) {
  
  datos_vivienda <- italia_plot_data %>% filter(!is.na(Precios_vivienda_pct_cambio))
  
  p15_crecimiento_vivienda <- plot_ly(datos_vivienda) %>%
    add_trace(x = ~Fecha, y = ~Precios_vivienda_pct_cambio, 
              type = 'bar',
              name = 'Crecimiento Precios Vivienda',
              marker = list(
                color = ~ifelse(Precios_vivienda_pct_cambio >= 0, PANTONE_376_C, PANTONE_220_C),
                opacity = 0.7
              ),
              hovertemplate = paste(
                "<b>Año: %{x|%Y}</b><br>",
                "<b>%{customdata}</b><br>",
                "<b>Crecimiento:</b> %{text}<br>",
                "<extra></extra>"
              ),
              customdata = ~Trimestre_Label,
              text = ~Precios_vivienda_pct_cambio_formatted,
              showlegend = FALSE,
              textposition = 'none') %>%  # Eliminar etiquetas en barras
    add_trace(x = ~Fecha, y = ~mean(Precios_vivienda_pct_cambio, na.rm = TRUE),
              type = 'scatter', mode = 'lines',
              name = 'Media',
              line = list(color = PANTONE_262_C, width = 2, dash = 'dash'),
              hovertemplate = paste(
                "<b>Media crecimiento:</b> %{text}<br>",
                "<extra></extra>"
              ),
              text = ~formatear_porcentaje(mean(Precios_vivienda_pct_cambio, na.rm = TRUE)),
              showlegend = TRUE) %>%
    layout(
      title = list(
        text = "<b>Crecimiento Trimestral de Precios de Vivienda - Italia</b>",
        x = 0.5,
        font = list(color = PANTONE_220_C, size = 16)
      ),
      xaxis = crear_eje_x_anual(datos_vivienda),
      yaxis = list(
        title = "Cambio Porcentual Trimestral (%)",
        gridcolor = PANTONE_9043_C,
        titlefont = list(color = PANTONE_220_C),
        tickfont = list(color = PANTONE_220_C)
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = PANTONE_220_C,
        font = list(color = PANTONE_220_C)
      )
    ) %>%
    config(config_comun)
  
  print(p15_crecimiento_vivienda)
  cat("✓ Gráfico de crecimiento de precios de vivienda creado (sin etiquetas en barras)\n")
}

# =============================================================================
# EXPORTACIÓN
# =============================================================================

# Función para exportar un gráfico a PDF
exportar_a_pdf <- function(grafico, nombre_archivo, ancho = 12, alto = 8) {
  tryCatch({
    # Crear archivo HTML temporal
    archivo_temp <- tempfile(fileext = ".html")
    
    # Guardar como HTML
    htmlwidgets::saveWidget(grafico, archivo_temp, selfcontained = TRUE)
    
    # Convertir a PDF - usando cliprect para recortar al área del gráfico
    pdf_file <- paste0("Graficos/", nombre_archivo, ".pdf")
    webshot2::webshot(archivo_temp, pdf_file, 
                      vwidth = ancho * 100, 
                      vheight = alto * 100,
                      cliprect = "viewport")  # Esto recorta al viewport del gráfico
    
    cat("✓ Gráfico exportado:", pdf_file, "\n")
    
    # Eliminar archivo temporal
    if (file.exists(archivo_temp)) {
      file.remove(archivo_temp)
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error exportando", nombre_archivo, ":", e$message, "\n")
    
    # Intentar método alternativo si el primero falla
    try({
      cat("Intentando método alternativo...\n")
      archivo_temp <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(grafico, archivo_temp, selfcontained = TRUE)
      
      webshot2::webshot(archivo_temp, pdf_file, 
                        cliprect = "viewport")
      
      if (file.exists(pdf_file)) {
        cat("✓ Gráfico exportado (método alternativo):", pdf_file, "\n")
        file.remove(archivo_temp)
        return(TRUE)
      }
    }, silent = TRUE)
    
    return(FALSE)
  })
}

# Función alternativa para gráficos específicos que puedan necesitar ajustes
exportar_a_pdf_ajustado <- function(grafico, nombre_archivo, ancho = 10, alto = 6) {
  tryCatch({
    # Crear archivo HTML temporal
    archivo_temp <- tempfile(fileext = ".html")
    
    # Guardar como HTML
    htmlwidgets::saveWidget(grafico, archivo_temp, selfcontained = TRUE)
    
    # Convertir a PDF con ajustes más agresivos
    pdf_file <- paste0("Graficos/", nombre_archivo, ".pdf")
    
    # Intentar diferentes métodos de recorte
    webshot2::webshot(archivo_temp, pdf_file, 
                      vwidth = ancho * 100, 
                      vheight = alto * 100,
                      cliprect = "viewport",
                      expand = c(-10, -10, -10, -10))  # Reducir márgenes
    
    cat("✓ Gráfico exportado (ajustado):", pdf_file, "\n")
    
    # Eliminar archivo temporal
    if (file.exists(archivo_temp)) {
      file.remove(archivo_temp)
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error exportando", nombre_archivo, ":", e$message, "\n")
    return(FALSE)
  })
}

# Función principal para exportar todos los gráficos a PDF
exportar_todos_los_graficos_pdf <- function() {
  cat("=== INICIANDO EXPORTACIÓN DE TODOS LOS GRÁFICOS A PDF ===\n\n")
  
  # Crear directorio Graficos si no existe
  dir.create("Graficos", showWarnings = FALSE)
  
  # Lista para almacenar resultados
  resultados <- list()
  contador <- 0
  
  # Exportar cada gráfico con la función mejorada
  
  if(exists("p1_pib_nominal")) {
    resultados$pib_nominal <- exportar_a_pdf(p1_pib_nominal, "01_PIB_Nominal_Italia", 11, 7)
    if(resultados$pib_nominal) contador <- contador + 1
  }
  
  if(exists("p2_crecimiento_pib")) {
    resultados$crecimiento_pib <- exportar_a_pdf(p2_crecimiento_pib, "02_Crecimiento_PIB_Italia", 11, 7)
    if(resultados$crecimiento_pib) contador <- contador + 1
  }
  
  if(exists("p3_deflactor")) {
    resultados$deflactor <- exportar_a_pdf(p3_deflactor, "03_Deflactor_PIB_Italia", 11, 7)
    if(resultados$deflactor) contador <- contador + 1
  }
  
  if(exists("p4_inflacion")) {
    resultados$inflacion <- exportar_a_pdf(p4_inflacion, "04_IPC_Italia", 11, 7)
    if(resultados$inflacion) contador <- contador + 1
  }
  
  if(exists("p5_inflacion_trimestral")) {
    resultados$inflacion_trimestral <- exportar_a_pdf(p5_inflacion_trimestral, "05_Inflacion_Trimestral_Italia", 11, 7)
    if(resultados$inflacion_trimestral) contador <- contador + 1
  }
  
  if(exists("p6_empleo")) {
    resultados$empleo <- exportar_a_pdf(p6_empleo, "06_Empleo_Desempleo_Italia", 11, 7)
    if(resultados$empleo) contador <- contador + 1
  }
  
  if(exists("p7_salarios")) {
    resultados$salarios <- exportar_a_pdf(p7_salarios, "07_Salarios_Italia", 11, 7)
    if(resultados$salarios) contador <- contador + 1
  }
  
  if(exists("p8_productividad")) {
    resultados$productividad <- exportar_a_pdf(p8_productividad, "08_Productividad_Laboral_Italia", 11, 7)
    if(resultados$productividad) contador <- contador + 1
  }
  
  if(exists("p9_comercio")) {
    resultados$comercio <- exportar_a_pdf(p9_comercio, "09_Comercio_Exterior_Italia", 11, 7)
    if(resultados$comercio) contador <- contador + 1
  }
  
  if(exists("p10_balanza")) {
    resultados$balanza <- exportar_a_pdf(p10_balanza, "10_Balanza_Comercial_Italia", 11, 7)
    if(resultados$balanza) contador <- contador + 1
  }
  
  if(exists("p11_finanzas")) {
    resultados$finanzas <- exportar_a_pdf(p11_finanzas, "11_Finanzas_Publicas_Italia", 11, 7)
    if(resultados$finanzas) contador <- contador + 1
  }
  
  if(exists("p12_deficit")) {
    resultados$deficit <- exportar_a_pdf(p12_deficit, "12_Deficit_Superavit_Italia", 11, 7)
    if(resultados$deficit) contador <- contador + 1
  }
  
  if(exists("p13_interes")) {
    resultados$interes <- exportar_a_pdf(p13_interes, "13_Tipo_Interes_Italia", 11, 7)
    if(resultados$interes) contador <- contador + 1
  }
  
  if(exists("p14_confianza")) {
    resultados$confianza <- exportar_a_pdf(p14_confianza, "14_Confianza_Consumidor_Italia", 11, 7)
    if(resultados$confianza) contador <- contador + 1
  }
  
  if(exists("p15_crecimiento_vivienda")) {
    resultados$vivienda <- exportar_a_pdf(p15_crecimiento_vivienda, "15_Precios_Vivienda_Italia", 11, 7)
    if(resultados$vivienda) contador <- contador + 1
  }
  
  cat("\n=== RESUMEN DE EXPORTACIÓN ===\n")
  cat("Gráficos exportados exitosamente:", contador, "de", length(resultados), "\n")
  
  return(resultados)
}

# Ejecutar la función para exportar todos los gráficos a PDF
resultados_exportacion <- exportar_todos_los_graficos_pdf()

# Verificar si hay errores
if (sum(unlist(resultados_exportacion)) < length(resultados_exportacion)) {
  cat("\nALERTA: Algunos gráficos no se pudieron exportar.\n")
  cat("Gráficos con problemas:\n")
  for (nombre in names(resultados_exportacion)) {
    if (!resultados_exportacion[[nombre]]) {
      cat(" -", nombre, "\n")
    }
  }
} else {
  cat("\n¡Todos los gráficos se exportaron exitosamente a PDF!\n")
}

# Función adicional para verificar el tamaño de los PDFs generados
verificar_tamanos_pdf <- function() {
  cat("\n=== VERIFICANDO TAMAÑOS DE PDFs ===\n")
  archivos_pdf <- list.files("Graficos", pattern = "\\.pdf$", full.names = TRUE)
  
  for (archivo in archivos_pdf) {
    info <- file.info(archivo)
    cat(basename(archivo), ": ", round(info$size/1024, 1), "KB\n", sep = "")
  }
}

# Ejecutar verificación de tamaños
verificar_tamanos_pdf()
