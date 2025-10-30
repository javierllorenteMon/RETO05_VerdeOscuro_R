IPC_sinO_M
IPC_2022Q4_Reales <- read.csv("Datos/Istat/IPC_2022Q4_Reales.csv")
predicciones <- readRDS("Datos/Resultados/Pred_IPC_2022_Q4.rds")


library(dplyr)
library(ggplot2)

IPC_sinO_M <- readRDS("Datos/transformados/IPC_sinO_M.rds")
IPC_sinO   <- readRDS("Datos/transformados/IPC_sinO.rds")
IPC_2022Q4_Reales <- read.csv("Datos/Istat/IPC_2022Q4_Reales.csv")
predicciones <- readRDS("Datos/Resultados/Pred_IPC_2022_Q4.rds")


IPC_2022_real_vec <- as.numeric(window(IPC_sinO_M, start=c(2022,1), end=c(2022,9)))

# Último mes de 2021
ultimo_mes_2021 <- as.numeric(window(IPC_sinO_M, start=c(2021,12), end=c(2021,12)))

# ------------------------------
# 2. Datos reales Q4 2022 del CSV (base 2015 → base 2016)
# ------------------------------
IPC_2022Q4_Reales_sub <- IPC_2022Q4_Reales %>%
  filter(TIME_PERIOD %in% c("2022-10","2022-11","2022-12"))

# Base 2016 de referencia
base_2016 <- as.numeric(window(IPC_sinO_M, start=c(2016,12), end=c(2016,12)))

# Escalar de base 2015 → base 2016
IPC_2022Q4_scaled <- IPC_2022Q4_Reales_sub$Observation * base_2016 / 100

# ------------------------------
# 3. Vector completo de IPC real y predicción
# ------------------------------
IPC_2022_real_total <- c(IPC_2022_real_vec, IPC_2022Q4_scaled)
IPC_2022_pred_vec  <- predicciones$Pred_ARIMA_Manual

# ------------------------------
# 4. Data frame completo
# ------------------------------
meses <- month.abb

IPC_2022_df <- data.frame(
  Mes = meses,
  Fecha = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by="month"),
  IPC_Real = IPC_2022_real_total,
  IPC_Pred = c(rep(NA,9), IPC_2022_pred_vec)
)

# ------------------------------
# 5. Incrementos mensuales
# ------------------------------
# Incrementos reales
IPC_2022_vec_ext <- c(ultimo_mes_2021, IPC_2022_real_total)
IPC_2022_df$Pct_Mes <- round(100 * diff(IPC_2022_vec_ext) / IPC_2022_vec_ext[-length(IPC_2022_vec_ext)], 2)

# Incrementos predichos (solo Q4)
ultimo_real <- IPC_2022_real_vec[length(IPC_2022_real_vec)]
IPC_2022_df$Pct_Pred <- c(rep(NA, 9),
                          round(100 * diff(c(ultimo_real, IPC_2022_pred_vec)) /
                                  c(ultimo_real, IPC_2022_pred_vec[-length(IPC_2022_pred_vec)]), 2))

# ------------------------------
# 6. Incrementos trimestrales (incluyendo Q4 2022 reales)
# ------------------------------
# Promedios trimestrales
Q4_2021 <- as.numeric(window(IPC_sinO, start=c(2021,4), end=c(2021,4)))
Qtr_2022 <- as.numeric(window(IPC_sinO, start=c(2022,1), end=c(2022,3)))
Q4_2022_real <- mean(IPC_2022Q4_scaled)

Qtr_vec_ext_full <- c(Q4_2021, Qtr_2022, Q4_2022_real)
Pct_Trim_full <- round(100 * diff(Qtr_vec_ext_full) / Qtr_vec_ext_full[-length(Qtr_vec_ext_full)], 2)

# Cada trimestre tiene 3 meses
IPC_2022_df$Pct_Trim <- rep(Pct_Trim_full, each=3)


ggplot(IPC_2022_df, aes(x=Fecha)) +
  geom_line(aes(y=Pct_Mes, color="Real"), size=1.2) +
  geom_point(aes(y=Pct_Mes, color="Real"), size=3) +
  geom_line(aes(y=Pct_Pred, color="Predicción"), size=1.2, linetype="dashed", na.rm=TRUE) +
  geom_point(aes(y=Pct_Pred, color="Predicción"), size=3, na.rm=TRUE) +
  geom_line(aes(y=Pct_Trim), color="green", size=1.2, na.rm=TRUE) +
  geom_point(aes(y=Pct_Trim), color="green", size=3, na.rm=TRUE) +
  scale_color_manual(values=c("Real"="blue","Predicción"="red")) +
  scale_x_date(date_breaks="1 month", date_labels="%b", limits=as.Date(c("2022-01-01","2022-12-31"))) +
  labs(
    title="Incremento mensual y trimestral del IPC 2022 (referencia 2021)",
    x="Mes",
    y="Incremento (%)",
    color="Tipo de dato",
    caption="Azul: mensual real | Rojo: mensual | Verde: trimestral"
  ) +
  theme_minimal(base_size=14) +
  theme(axis.text.x = element_text(angle=45, hjust=1))


IPC_2022_df

library(dplyr)
library(zoo)  # para as.yearmon

# --- 1️⃣ Extraer los años disponibles en el CSV de ISTAT ---
ipc_istat_hist <- IPC_2022Q4_Reales %>%
  filter(TIME_PERIOD >= "2019-01" & TIME_PERIOD <= "2021-12") %>%
  mutate(
    Fecha = as.yearmon(TIME_PERIOD),
    Observation = as.numeric(Observation)  # aseguramos que es numérico
  )

# --- 2️⃣ Extraer los mismos meses de tu serie mensual base ---
ipc_modelo_hist <- window(IPC_sinO_M, start=c(2019,1), end=c(2021,12))
ipc_modelo_df <- data.frame(
  Fecha = as.yearmon(time(ipc_modelo_hist)),
  IPC_sinO_M = as.numeric(ipc_modelo_hist)
)

# --- 3️⃣ Unir por fecha para comparar ---
comparacion_df <- merge(ipc_modelo_df, ipc_istat_hist[, c("Fecha", "Observation")], by="Fecha", all=FALSE)
colnames(comparacion_df)[3] <- "IPC_ISTAT"

# --- 4️⃣ Calcular ratio entre ambas series ---
comparacion_df <- comparacion_df %>%
  mutate(
    IPC_sinO_M = as.numeric(IPC_sinO_M),
    IPC_ISTAT = as.numeric(IPC_ISTAT),
    Ratio = IPC_ISTAT / IPC_sinO_M
  )

# --- 5️⃣ Resumen estadístico del ratio ---
resumen_ratio <- comparacion_df %>%
  summarise(
    media = mean(Ratio, na.rm=TRUE),
    sd = sd(Ratio, na.rm=TRUE),
    min = min(Ratio, na.rm=TRUE),
    max = max(Ratio, na.rm=TRUE)
  )

# --- 6️⃣ Mostrar resultados ---
print("🔍 Comparación entre IPC_sinO_M (tu serie) y IPC_2022Q4_Reales (ISTAT) para 2019–2021")
print(head(comparacion_df, 12))
print("📊 Resumen del ratio entre series:")
print(resumen_ratio)
