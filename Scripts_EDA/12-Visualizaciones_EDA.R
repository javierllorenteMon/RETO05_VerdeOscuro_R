# Cargar librerías adicionales
library(plotly)
library(gridExtra)
library(scales)

# Cargar los datos
italia_data <- read.csv("Datos/italia_trimestral_completo_definitivo.csv")

# Crear variable Year para agrupar y mostrar solo años en el eje X
italia_data <- italia_data %>%
  mutate(Year = as.factor(Year),
         Periodo_num = as.numeric(factor(Periodo, levels = unique(Periodo))))

# 1. GRÁFICOS DE VARIABLES MACROECONÓMICAS PRINCIPALES

# PIB a lo largo del tiempo
p1 <- ggplot(italia_data, aes(x = Periodo_num, y = GDP.billion.currency.units, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 1) +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral del PIB de Italia",
       x = "Año", y = "PIB (miles de millones de unidades monetarias)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1_plotly <- ggplotly(p1) %>% layout(xaxis = list(tickangle = -45))

# IPC a lo largo del tiempo
p2 <- ggplot(italia_data, aes(x = Periodo_num, y = Consumer.Price.Index..CPI., group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred", size = 1) +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral del Índice de Precios al Consumidor (IPC)",
       x = "Año", y = "IPC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2_plotly <- ggplotly(p2) %>% layout(xaxis = list(tickangle = -45))

# 2. GRÁFICOS DE EMPLEO Y SALARIOS

# Índice de empleo
p3 <- ggplot(italia_data, aes(x = Periodo_num, y = Indice_empleo, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 1) +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral del Índice de Empleo",
       x = "Año", y = "Índice de Empleo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3_plotly <- ggplotly(p3) %>% layout(xaxis = list(tickangle = -45))

# Tasa de empleo y desempleo (si existen)
if(all(c("EMPLEO", "DESEMPLEO") %in% names(italia_data))) {
  empleo_desempleo <- italia_data %>%
    select(Periodo_num, Year, EMPLEO, DESEMPLEO) %>%
    pivot_longer(cols = c(EMPLEO, DESEMPLEO), names_to = "Variable", values_to = "Valor")
  
  p4 <- ggplot(empleo_desempleo, aes(x = Periodo_num, y = Valor, color = Variable, group = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                       labels = unique(italia_data$Year)) +
    labs(title = "Evolución Trimestral de Tasas de Empleo y Desempleo",
         x = "Año", y = "Porcentaje (%)") +
    scale_color_manual(values = c("EMPLEO" = "blue", "DESEMPLEO" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
  p4_plotly <- ggplotly(p4) %>% layout(xaxis = list(tickangle = -45))
}

# Índice de salarios
p5 <- ggplot(italia_data, aes(x = Periodo_num, y = Indice_salarios, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple", size = 1) +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral del Índice de Salarios",
       x = "Año", y = "Índice de Salarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5_plotly <- ggplotly(p5) %>% layout(xaxis = list(tickangle = -45))

# 3. GRÁFICOS DE COMERCIO EXTERIOR

# Exportaciones e importaciones
if(all(c("exportaciones", "importaciones") %in% names(italia_data))) {
  comercio_data <- italia_data %>%
    select(Periodo_num, Year, exportaciones, importaciones) %>%
    pivot_longer(cols = c(exportaciones, importaciones), names_to = "Variable", values_to = "Valor")
  
  p6 <- ggplot(comercio_data, aes(x = Periodo_num, y = Valor, color = Variable, group = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                       labels = unique(italia_data$Year)) +
    labs(title = "Evolución Trimestral de Exportaciones e Importaciones",
         x = "Año", y = "Valor") +
    scale_color_manual(values = c("exportaciones" = "darkgreen", "importaciones" = "darkorange")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top") +
    scale_y_continuous(labels = comma)
  
  p6_plotly <- ggplotly(p6) %>% layout(xaxis = list(tickangle = -45))
}

# Balanza comercial (calculada)
p7 <- italia_data %>%
  mutate(Balanza_comercial = exportaciones - importaciones) %>%
  ggplot(aes(x = Periodo_num, y = Balanza_comercial, group = 1)) +
  geom_line(color = "brown", size = 1.2) +
  geom_point(color = "brown", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Balanza Comercial Trimestral (Exportaciones - Importaciones)",
       x = "Año", y = "Balanza Comercial") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

p7_plotly <- ggplotly(p7) %>% layout(xaxis = list(tickangle = -45))

# 4. GRÁFICOS DE FINANZAS PÚBLICAS

# Ingresos vs Gastos del gobierno
if(all(c("Total.government.revenue", "Total.Government.expenditure") %in% names(italia_data))) {
  finanzas_data <- italia_data %>%
    select(Periodo_num, Year, Total.government.revenue, Total.Government.expenditure) %>%
    pivot_longer(cols = c(Total.government.revenue, Total.Government.expenditure), 
                 names_to = "Variable", values_to = "Valor")
  
  p8 <- ggplot(finanzas_data, aes(x = Periodo_num, y = Valor, color = Variable, group = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                       labels = unique(italia_data$Year)) +
    labs(title = "Evolución Trimestral de Ingresos y Gastos del Gobierno",
         x = "Año", y = "Valor") +
    scale_color_manual(values = c("Total.government.revenue" = "darkblue", 
                                  "Total.Government.expenditure" = "darkred"),
                       labels = c("Ingresos", "Gastos")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top") +
    scale_y_continuous(labels = comma)
  
  p8_plotly <- ggplotly(p8) %>% layout(xaxis = list(tickangle = -45))
}

# Déficit/Superávit
p9 <- ggplot(italia_data, aes(x = Periodo_num, y = Deficit_Surplus, group = 1)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "orange", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral del Déficit/Superávit Público",
       x = "Año", y = "Déficit/Superávit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

p9_plotly <- ggplotly(p9) %>% layout(xaxis = list(tickangle = -45))

# Déficit como porcentaje del PIB
p10 <- ggplot(italia_data, aes(x = Periodo_num, y = Deficit_Surplus_Pct_PIB, group = 1)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(color = "darkorange", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Déficit/Superávit Trimestral como Porcentaje del PIB",
       x = "Año", y = "Porcentaje del PIB (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p10_plotly <- ggplotly(p10) %>% layout(xaxis = list(tickangle = -45))

# 5. GRÁFICOS DE PRODUCTIVIDAD

# Productividad laboral
p11 <- ggplot(italia_data, aes(x = Periodo_num, y = Productividad_laboral, group = 1)) +
  geom_line(color = "darkcyan", size = 1.2) +
  geom_point(color = "darkcyan", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Evolución Trimestral de la Productividad Laboral",
       x = "Año", y = "Productividad Laboral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p11_plotly <- ggplotly(p11) %>% layout(xaxis = list(tickangle = -45))

# 6. GRÁFICOS COMBINADOS Y RELACIONES

# PIB vs IPC (scatter plot) - Este no necesita cambio de eje X
p12 <- ggplot(italia_data, aes(x = GDP.billion.currency.units, y = Consumer.Price.Index..CPI.)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación entre PIB e IPC",
       x = "PIB (miles de millones)", y = "IPC") +
  theme_minimal()

p12_plotly <- ggplotly(p12)

# Empleo vs Salarios
p13 <- ggplot(italia_data, aes(x = Indice_empleo, y = Indice_salarios)) +
  geom_point(color = "purple", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Relación entre Empleo y Salarios",
       x = "Índice de Empleo", y = "Índice de Salarios") +
  theme_minimal()

p13_plotly <- ggplotly(p13)

# Exportaciones vs PIB
p14 <- ggplot(italia_data, aes(x = GDP.billion.currency.units, y = exportaciones)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación entre PIB y Exportaciones",
       x = "PIB (miles de millones)", y = "Exportaciones") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

p14_plotly <- ggplotly(p14)

# 7. GRÁFICOS DE DISTRIBUCIÓN (Estos no cambian)

# Histograma del PIB
p15 <- ggplot(italia_data, aes(x = GDP.billion.currency.units)) +
  geom_histogram(fill = "steelblue", color = "white", alpha = 0.8, bins = 20) +
  labs(title = "Distribución del PIB",
       x = "PIB (miles de millones)", y = "Frecuencia") +
  theme_minimal()

p15_plotly <- ggplotly(p15)

# Boxplot de variables principales
vars_boxplot <- italia_data %>%
  select(GDP.billion.currency.units, Consumer.Price.Index..CPI., 
         Indice_empleo, Indice_salarios, Productividad_laboral) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

p16 <- ggplot(vars_boxplot, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Variables Principales",
       x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

p16_plotly <- ggplotly(p16) %>% layout(xaxis = list(tickangle = -45))

# 8. GRÁFICOS DE TENDENCIAS TEMPORALES MÚLTIPLES

# Múltiples variables económicas normalizadas
trend_data <- italia_data %>%
  select(Periodo_num, Year, GDP.billion.currency.units, Consumer.Price.Index..CPI., 
         Indice_empleo, Indice_salarios) %>%
  mutate(across(where(is.numeric) & !c(Periodo_num, Year), scale)) %>%  # Normalizar excluyendo Periodo_num y Year
  pivot_longer(cols = -c(Periodo_num, Year), names_to = "Variable", values_to = "Valor")

p17 <- ggplot(trend_data, aes(x = Periodo_num, y = Valor, color = Variable, group = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 0.8) +
  scale_x_continuous(breaks = seq(1, nrow(italia_data), by = 4),
                     labels = unique(italia_data$Year)) +
  labs(title = "Tendencias Trimestrales de Variables Económicas (Normalizadas)",
       x = "Año", y = "Valor Normalizado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_color_brewer(palette = "Set1")

p17_plotly <- ggplotly(p17) %>% layout(xaxis = list(tickangle = -45))

# 9. GRÁFICO DE CORRELACIONES MEJORADO (Este no cambia)

variables_numericas <- italia_data %>%
  select(where(is.numeric)) %>%
  select(-Year, -Periodo_num) %>%
  select_if(~sum(!is.na(.)) > 10)

if(ncol(variables_numericas) > 2) {
  cor_matrix <- cor(variables_numericas, use = "pairwise.complete.obs")
  
  # Convertir matriz de correlación a formato largo para ggplot
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Var1", "Var2", "Correlation")
  
  p18 <- ggplot(cor_data, aes(Var1, Var2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = round(Correlation, 2)), color = "white", size = 3) +
    scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Matriz de Correlación - Italia",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p18_plotly <- ggplotly(p18) %>% layout(xaxis = list(tickangle = -45))
}

# 10. GRÁFICO DE EVOLUCIÓN ANUAL

# Resumen anual
annual_summary <- italia_data %>%
  group_by(Year) %>%
  summarise(
    PIB_promedio = mean(GDP.billion.currency.units, na.rm = TRUE),
    IPC_promedio = mean(Consumer.Price.Index..CPI., na.rm = TRUE),
    Empleo_promedio = mean(Indice_empleo, na.rm = TRUE),
    Salarios_promedio = mean(Indice_salarios, na.rm = TRUE),
    .groups = 'drop'
  )

p19 <- annual_summary %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Year, y = Valor, color = Variable, group = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  labs(title = "Evolución Anual Promedio de Variables Principales",
       x = "Año", y = "Valor Promedio") +
  theme_minimal() +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme(legend.position = "none")

p19_plotly <- ggplotly(p19)

# MOSTRAR TODOS LOS GRÁFICOS
cat("=== GRÁFICOS GGPLOT2 ===\n")
print(p1)
print(p2)
print(p3)
if(exists("p4")) print(p4)
print(p5)
if(exists("p6")) print(p6)
print(p7)
if(exists("p8")) print(p8)
print(p9)
print(p10)
print(p11)
print(p12)
print(p13)
print(p14)
print(p15)
print(p16)
print(p17)
if(exists("p18")) print(p18)
print(p19)

cat("=== GRÁFICOS PLOTLY (Interactivos) ===\n")
# Los gráficos Plotly se pueden visualizar individualmente o en un dashboard
p1_plotly
p2_plotly
p3_plotly
if(exists("p4_plotly")) p4_plotly
p5_plotly
if(exists("p6_plotly")) p6_plotly
p7_plotly
if(exists("p8_plotly")) p8_plotly
p9_plotly
p10_plotly
p11_plotly
p12_plotly
p13_plotly
p14_plotly
p15_plotly
p16_plotly
p17_plotly
if(exists("p18_plotly")) p18_plotly
p19_plotly

# GUARDAR GRÁFICOS PRINCIPALES
cat("=== GUARDANDO GRÁFICOS ===\n")
#ggsave("graficos/01_pib_evolucion.png", p1, width = 12, height = 6)
#ggsave("graficos/02_ipc_evolucion.png", p2, width = 12, height = 6)
#ggsave("graficos/03_empleo_salarios.png", p3, width = 12, height = 6)
#ggsave("graficos/04_comercio_exterior.png", p7, width = 12, height = 6)
#ggsave("graficos/05_finanzas_publicas.png", p9, width = 12, height = 6)
#if(exists("p18")) ggsave("graficos/06_correlaciones.png", p18, width = 10, height = 8)

cat("✓ Todos los gráficos generados y guardados\n")