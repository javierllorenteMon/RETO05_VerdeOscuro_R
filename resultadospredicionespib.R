library(dplyr)
library(zoo)
library(ggplot2)

# --- Extraer PIB histórico de PIB_sinO (ts) ---
PIB_vals <- window(PIB_sinO, start=c(2021,2), end=c(2022,2))
fechas_hist <- as.yearqtr(time(PIB_vals))

PIB_hist_df <- data.frame(
  Fecha = fechas_hist,
  PIB_Real = as.numeric(PIB_vals)
)

# --- Datos reales Q3 y Q4 de 2022 ---
PIB_real_Q3_Q4 <- data.frame(
  Fecha = as.yearqtr(c("2022 Q3", "2022 Q4")),
  PIB_Real = c(510.632, 527.602)
)

# Unir histórico + últimos datos reales
PIB_total_real <- bind_rows(PIB_hist_df, PIB_real_Q3_Q4) %>%
  arrange(Fecha)

# --- Predicciones Q3 y Q4 2022 ---
Pred_PIB_Q3_Q4 <- data.frame(
  Fecha = as.yearqtr(c("2022 Q3", "2022 Q4")),
  PIB_Prediccion = c(475.8786, 510.6689)
)

# Unir predicciones con PIB real
PIB_comparacion <- PIB_total_real %>%
  left_join(Pred_PIB_Q3_Q4, by="Fecha") %>%
  # Calcular incrementos trimestrales
  mutate(
    Pct_Real = round(100 * c(NA, diff(PIB_Real)/PIB_Real[-length(PIB_Real)]), 2),
    Pct_Prediccion = round(100 * (PIB_Prediccion - lag(PIB_Real)) / lag(PIB_Real), 2)
  )

# Crear vector de breaks para cada punto
breaks_vec <- PIB_comparacion$Fecha

# --- Gráfico ---
ggplot(PIB_comparacion, aes(x=Fecha)) +
  geom_line(aes(y=Pct_Real, color="Histórico"), size=1.2) +
  geom_point(aes(y=Pct_Real, color="Histórico"), size=3) +
  geom_line(aes(y=Pct_Prediccion, color="Predicción"), size=1.2, linetype="dashed") +
  geom_point(aes(y=Pct_Prediccion, color="Predicción"), size=3) +
  scale_x_yearqtr(breaks = breaks_vec, format = "%Y-Q%q") +
  scale_color_manual(values=c("Histórico"="blue", "Predicción"="red")) +
  labs(title="Incremento trimestral del PIB 2021-Q2 a 2022-Q4 (Histórico + Predicciones)",
       x="Año-Trimestre", y="Incremento (%)", color="Tipo") +
  theme_minimal(base_size=14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

