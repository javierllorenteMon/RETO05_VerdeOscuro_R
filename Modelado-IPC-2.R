# =====================================================================
# IPC-fusion.R  —  Flujo comparado (log+diff+revert) vs (Box-Cox + biasadj)
# =====================================================================

library(dplyr)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)
library(openxlsx)  

IPC_sinO <- readRDS("Datos/transformados/IPC_sinO_M.rds")
stopifnot(frequency(IPC_sinO) == 12)

#decomIPC <- decompose(IPC_sinO); print(autoplot(decomIPC))

# -------------------- Train / Test -----------------
train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2021, 12))
test_IPC  <- window(IPC_sinO, start = c(2022, 1), end = c(2022, 9))
h_test    <- length(test_IPC)

# Llamar a tu función si existe (para cumplir requisitos)
if (exists("test_estacionariedad")) {
  test_estacionariedad(train_IPC, nombre = "IPC (train)")
}

# =====================================================================
# PARTE A) log + diff(d=2) sobre TRAIN, modelar y revertir
# =====================================================================

# 1) Estacionarizar SOLO el train
tsdisplay(train_IPC)
train_IPC_est <- diff(log(train_IPC), differences = 2)
tsdisplay(train_IPC_est)

# 2) Modelos en serie estacionaria
set.seed(123)
modelo_IPC_Arima  <- auto.arima(train_IPC_est, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
modelo_IPC_Sarima <- auto.arima(train_IPC_est, seasonal = TRUE,  stepwise = FALSE, approximation = FALSE)

summary(modelo_IPC_Arima);  checkresiduals(modelo_IPC_Arima)
summary(modelo_IPC_Sarima); checkresiduals(modelo_IPC_Sarima)

# 3) Forecast en escala estacionaria
fc_A_t <- forecast(modelo_IPC_Arima,  h = h_test)
fc_S_t <- forecast(modelo_IPC_Sarima, h = h_test)

# 4) Revertir a escala original (Diapo 81-82): usamos ÚLTIMOS valores ORIGINALES (log)
tail_log <- log(tail(train_IPC, 2))

revert_pred <- function(fc_obj, xi_log, d = 2) {
  mu   <- diffinv(fc_obj$mean,            differences = d, xi = xi_log)
  lo80 <- diffinv(fc_obj$lower[,"80%"],   differences = d, xi = xi_log)
  up80 <- diffinv(fc_obj$upper[,"80%"],   differences = d, xi = xi_log)
  lo95 <- diffinv(fc_obj$lower[,"95%"],   differences = d, xi = xi_log)
  up95 <- diffinv(fc_obj$upper[,"95%"],   differences = d, xi = xi_log)
  dropn <- d
  list(
    mean = exp(mu[-seq_len(dropn)]),
    lo80 = exp(lo80[-seq_len(dropn)]),
    up80 = exp(up80[-seq_len(dropn)]),
    lo95 = exp(lo95[-seq_len(dropn)]),
    up95 = exp(up95[-seq_len(dropn)])
  )
}

rev_A <- revert_pred(fc_A_t, tail_log, d = 2)
rev_S <- revert_pred(fc_S_t, tail_log, d = 2)

pred_A <- ts(rev_A$mean, start = start(test_IPC), frequency = 12)
pred_S <- ts(rev_S$mean, start = start(test_IPC), frequency = 12)

# 5) Accuracy en TEST
acc_A <- accuracy(pred_A, test_IPC)
acc_S <- accuracy(pred_S, test_IPC)
print(acc_A); print(acc_S)

# Benchmarks
acc_N  <- accuracy(naive(train_IPC,  h = h_test)$mean,  test_IPC)
acc_SN <- accuracy(snaive(train_IPC, h = h_test)$mean, test_IPC)
print(acc_N); print(acc_SN)

# 6) Gráficos comparativos (TEST)
Meses <- time(test_IPC)
df_test        <- data.frame(Mes = Meses, IPC = as.numeric(test_IPC))
df_pred_A      <- data.frame(Mes = Meses, Pred = as.numeric(pred_A))
df_pred_S      <- data.frame(Mes = Meses, Pred = as.numeric(pred_S))
df_naive       <- data.frame(Mes = Meses, Pred = as.numeric(naive(train_IPC,  h=h_test)$mean))
df_snaive      <- data.frame(Mes = Meses, Pred = as.numeric(snaive(train_IPC, h=h_test)$mean))

plot_A <- ggplot() +
  geom_line(data = df_test, aes(Mes, IPC), color="blue", size=1.1) +
  geom_line(data = df_pred_A, aes(Mes, Pred), color="red", linetype="dashed", size=1.1) +
  labs(title="ARIMA (log+diff) vs IPC real — Test 2022-01..09", x="Mes", y="IPC") +
  theme_minimal()

plot_S <- ggplot() +
  geom_line(data = df_test, aes(Mes, IPC), color="blue", size=1.1) +
  geom_line(data = df_pred_S, aes(Mes, Pred), color="green4", linetype="dashed", size=1.1) +
  labs(title="SARIMA (log+diff) vs IPC real — Test 2022-01..09", x="Mes", y="IPC") +
  theme_minimal()

grid.arrange(plot_A, plot_S, ncol = 1)

plot_all <- ggplot() +
  geom_line(data = df_test, aes(Mes, IPC), color="blue", size=1.1) +
  geom_line(data = df_pred_A, aes(Mes, Pred), color="red", linetype="dashed", size=1.1) +
  geom_line(data = df_pred_S, aes(Mes, Pred), color="green4", linetype="dashed", size=1.1) +
  geom_line(data = df_naive,  aes(Mes, Pred), color="purple", linetype="dotted", size=1.1) +
  geom_line(data = df_snaive, aes(Mes, Pred), color="orange", linetype="dotted", size=1.1) +
  labs(title="IPC Test — ARIMA, SARIMA, Naive y SNaive", x="Mes", y="IPC") +
  theme_minimal()
print(plot_all)


# =====================================================================
# PARTE B) Alternativa: Box-Cox (sin biasadj) — sin diffinv
# =====================================================================
set.seed(123)
lam <- BoxCox.lambda(train_IPC)  # SOLO con train
mod_BC <- auto.arima(train_IPC,
                     seasonal = TRUE,
                     lambda   = lam,         # aplica Box-Cox
                     stepwise = FALSE,
                     approximation = FALSE)

summary(mod_BC); checkresiduals(mod_BC)

fc_BC <- forecast(mod_BC, h = h_test)   # devuelve ya re-transformado (sin bias adj)
acc_BC <- accuracy(fc_BC, test_IPC); print(acc_BC)

df_BC <- data.frame(
  Mes  = time(test_IPC),
  Real = as.numeric(test_IPC),
  Pred = as.numeric(fc_BC$mean)
)

plot_BC <- ggplot()+
  geom_line(data=df_BC, aes(Mes, Real), color="blue", size=1.1) +
  geom_line(data=df_BC, aes(Mes, Pred), color="black", linetype="dashed", size=1.1) +
  labs(title="Auto ARIMA con Box-Cox (sin biasadj) vs IPC real — Test",
       x="Mes", y="IPC") +
  theme_minimal()
print(plot_BC)

# -------------------- Resumen de modelos (selección) ------------------
lb_pval <- function(m, lag=24){
  k <- length(coef(m))
  Box.test(residuals(m), lag=lag, type="Ljung-Box", fitdf=k)$p.value
}

resumen_modelos <- data.frame(
  Modelo     = c("ARIMA log+diff", "SARIMA log+diff", "Auto BC"),
  AICc       = c(modelo_IPC_Arima$aicc, modelo_IPC_Sarima$aicc, mod_BC$aicc),
  BIC        = c(modelo_IPC_Arima$bic,  modelo_IPC_Sarima$bic,  mod_BC$bic),
  LjungBox_p = c(lb_pval(modelo_IPC_Arima), lb_pval(modelo_IPC_Sarima), lb_pval(mod_BC)),
  RMSE       = c(acc_A["Test set","RMSE"],  acc_S["Test set","RMSE"],  acc_BC["Test set","RMSE"]),
  MAE        = c(acc_A["Test set","MAE"],   acc_S["Test set","MAE"],   acc_BC["Test set","MAE"]),
  MAPE       = c(acc_A["Test set","MAPE"],  acc_S["Test set","MAPE"],  acc_BC["Test set","MAPE"])
)
print(resumen_modelos)


# =====================================================================
# PARTE C) Predicción futura Q4 2022 (Oct-Nov-Dic) para los TRES enfoques
# =====================================================================

# 1) Reentrenar hasta sep-2022
train_full <- window(IPC_sinO, end = c(2022, 9))

# A. log+diff (ARIMA y SARIMA), revert + bandas
train_full_est <- diff(log(train_full), differences = 2)

mod_A_full <- auto.arima(train_full_est, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
mod_S_full <- auto.arima(train_full_est, seasonal=TRUE,  stepwise=FALSE, approximation=FALSE)

fc_A_q4_t <- forecast(mod_A_full, h=3)
fc_S_q4_t <- forecast(mod_S_full, h=3)

tail_log_full <- log(tail(train_full, 2))
rev_A_q4 <- revert_pred(fc_A_q4_t, tail_log_full, d=2)
rev_S_q4 <- revert_pred(fc_S_q4_t, tail_log_full, d=2)

df_pred_A_q4 <- data.frame(
  Fecha = time(ts(rev_A_q4$mean, start=c(2022,10), frequency=12)),
  Pred  = as.numeric(rev_A_q4$mean),
  LI80  = as.numeric(rev_A_q4$lo80),
  LS80  = as.numeric(rev_A_q4$up80)
)
df_pred_S_q4 <- data.frame(
  Fecha = time(ts(rev_S_q4$mean, start=c(2022,10), frequency=12)),
  Pred  = as.numeric(rev_S_q4$mean),
  LI80  = as.numeric(rev_S_q4$lo80),
  LS80  = as.numeric(rev_S_q4$up80)
)

# B. Auto BC (sin diffinv, sin biasadj)
lam_full <- BoxCox.lambda(train_full)
mod_BC_full <- auto.arima(train_full,
                          seasonal = TRUE,
                          lambda   = lam_full,
                          stepwise = FALSE,
                          approximation = FALSE)

fc_BC_q4 <- forecast(mod_BC_full, h=3)

df_pred_BC_q4 <- data.frame(
  Fecha = time(fc_BC_q4$mean),
  Pred  = as.numeric(fc_BC_q4$mean),
  LI80  = as.numeric(fc_BC_q4$lower[,"80%"]),
  LS80  = as.numeric(fc_BC_q4$upper[,"80%"])
)

# Histórico reciente para contexto
hist_win <- max(start(IPC_sinO)[1], 2019)
IPC_hist <- window(IPC_sinO, start=c(hist_win,1), end=c(2022,9))
df_hist <- data.frame(Fecha=time(IPC_hist), Valor=as.numeric(IPC_hist))

# Gráficos futuros (panel ARIMA/SARIMA)
plot_future_A <- ggplot()+
  geom_line(data=df_hist, aes(Fecha, Valor), color="blue", size=1.2) +
  geom_ribbon(data=df_pred_A_q4, aes(Fecha, ymin=LI80, ymax=LS80), alpha=0.15) +
  geom_line(data=df_pred_A_q4, aes(Fecha, Pred), color="red", linetype="dashed", size=1.2) +
  labs(title="IPC — Predicción ARIMA (Oct–Nov–Dic 2022)", x="Año", y="IPC")+
  theme_minimal()

plot_future_S <- ggplot()+
  geom_line(data=df_hist, aes(Fecha, Valor), color="blue", size=1.2) +
  geom_ribbon(data=df_pred_S_q4, aes(Fecha, ymin=LI80, ymax=LS80), alpha=0.15) +
  geom_line(data=df_pred_S_q4, aes(Fecha, Pred), color="green4", linetype="dashed", size=1.2) +
  labs(title="IPC — Predicción SARIMA (Oct–Nov–Dic 2022)", x="Año", y="IPC")+
  theme_minimal()

grid.arrange(plot_future_A, plot_future_S, ncol=1)

# Gráfico combinado con los tres (sin bandas para simplificar)
plot_future_all <- ggplot()+
  geom_line(data=df_hist, aes(Fecha, Valor), color="blue", size=1.2) +
  geom_line(data=df_pred_A_q4,  aes(Fecha, Pred), color="red",   linetype="dashed", size=1.2) +
  geom_line(data=df_pred_S_q4,  aes(Fecha, Pred), color="green4",linetype="dashed", size=1.2) +
  geom_line(data=df_pred_BC_q4, aes(Fecha, Pred), color="black", linetype="dashed", size=1.2) +
  labs(title="IPC — Predicción Oct–Nov–Dic 2022 (ARIMA, SARIMA, Auto BC)", x="Año", y="IPC")+
  theme_minimal()
print(plot_future_all)

# Tabla Q4 con los tres enfoques
pred_tabla_q4 <- data.frame(
  Mes   = c("2022-10","2022-11","2022-12"),
  ARIMA = round(df_pred_A_q4$Pred, 3),
  SARIMA= round(df_pred_S_q4$Pred, 3),
  AUTO_BC = round(df_pred_BC_q4$Pred, 3),
  ARIMA_LI80 = round(df_pred_A_q4$LI80, 3),
  ARIMA_LS80 = round(df_pred_A_q4$LS80, 3),
  SARIMA_LI80 = round(df_pred_S_q4$LI80, 3),
  SARIMA_LS80 = round(df_pred_S_q4$LS80, 3),
  BC_LI80 = round(df_pred_BC_q4$LI80, 3),
  BC_LS80 = round(df_pred_BC_q4$LS80, 3)
)
print(pred_tabla_q4)

# openxlsx::write.xlsx(pred_tabla_q4, "Pred_Q4_2022_IPC_fusion.xlsx", overwrite = TRUE)
