IPC_2022Q4_Reales <- read.csv("Datos/Istat/IPC_2022Q4_Reales.csv")
predicciones <- readRDS("Datos/Resultados/Pred_IPC_2022_Q4.rds")


library(dplyr)
library(ggplot2)

IPC_sinO_M <- readRDS("Datos/transformados/IPC_sinO_M.rds")
IPC_sinO   <- readRDS("Datos/transformados/IPC_sinO.rds")
IPC_2022Q4_Reales <- read.csv("Datos/Istat/IPC_2022Q4_Reales.csv")
predicciones <- readRDS("Datos/Resultados/Pred_IPC_2022_Q4.rds")


IPC_2022_real_vec <- as.numeric(window(IPC_sinO_M, start=c(2022,1), end=c(2022,9)))

ultimo_mes_2021 <- as.numeric(window(IPC_sinO_M, start=c(2021,12), end=c(2021,12)))

IPC_2022Q4_Reales_sub <- IPC_2022Q4_Reales %>%
  filter(TIME_PERIOD %in% c("2022-10","2022-11","2022-12"))

base_2016 <- as.numeric(window(IPC_sinO_M, start=c(2016,12), end=c(2016,12)))

IPC_2022Q4_scaled <- IPC_2022Q4_Reales_sub$Observation * base_2016 / 100


IPC_2022_real_total <- c(IPC_2022_real_vec, IPC_2022Q4_scaled)
IPC_2022_pred_vec  <- predicciones$Pred_ARIMA_Manual


meses <- month.abb

IPC_2022_df <- data.frame(
  Mes = meses,
  Fecha = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by="month"),
  IPC_Real = IPC_2022_real_total,
  IPC_Pred = c(rep(NA,9), IPC_2022_pred_vec)
)


IPC_2022_vec_ext <- c(ultimo_mes_2021, IPC_2022_real_total)
IPC_2022_df$Pct_Mes <- round(100 * diff(IPC_2022_vec_ext) / IPC_2022_vec_ext[-length(IPC_2022_vec_ext)], 2)

ultimo_real <- IPC_2022_real_vec[length(IPC_2022_real_vec)]
IPC_2022_df$Pct_Pred <- c(rep(NA, 9),
                          round(100 * diff(c(ultimo_real, IPC_2022_pred_vec)) /
                                  c(ultimo_real, IPC_2022_pred_vec[-length(IPC_2022_pred_vec)]), 2))


Q4_2021 <- as.numeric(window(IPC_sinO, start=c(2021,4), end=c(2021,4)))
Qtr_2022 <- as.numeric(window(IPC_sinO, start=c(2022,1), end=c(2022,3)))
Q4_2022_real <- mean(IPC_2022Q4_scaled)

Qtr_vec_ext_full <- c(Q4_2021, Qtr_2022, Q4_2022_real)
Pct_Trim_full <- round(100 * diff(Qtr_vec_ext_full) / Qtr_vec_ext_full[-length(Qtr_vec_ext_full)], 2)

IPC_2022_df$Pct_Trim <- rep(Pct_Trim_full, each=3)

IPC_2022_df
