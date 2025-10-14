### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL IPC (mensual)

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)
source("Scripts_Preprocesamiento/Funciones.R")

# Cargar fichero (ts mensual con frequency = 12)
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO_M.rds")
stopifnot(frequency(IPC_sinO) == 12)

# Descomposición de componentes
decomIPC <- decompose(IPC_sinO)
autoplot(decomIPC)

# Train(2000,1)(2021,12)  Test(2022,1)(2022,9)
train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2021, 12))
test_IPC  <- window(IPC_sinO, start = c(2022, 1), end = c(2022, 9))

# Comprobar estacionariedad (seguramente no estacionaria)
test_estacionariedad(train_IPC, nombre = "IPC")

# Comprobar si tiene varianza creciente para ver si aplicarle log
ts.plot(train_IPC)  # si parece tener varianza creciente, aplicamos log

# Estacionalidad (mensual, m=12): nsdiffs y ACF
nsdiffs(train_IPC)  # si devuelve 1, hay estacionalidad
acf(train_IPC)      # si hay picos en múltiplos de 12, sugiere estacionalidad

# Test de estacionariedad otra vez (ajustando la serie)
tsdisplay(train_IPC)
# Igual que tu compi: diferenciamos en log 2 veces (sin estacional explícita)
# (Si quisieras incluir estacionalidad: diff(diff(log(train_IPC)), lag = 12))
train_IPC_est <- diff(log(train_IPC), differences = 2)
tsdisplay(train_IPC_est)
test_estacionariedad(train_IPC_est, nombre = "IPC")

# =================== Modelos ===================

# ------ ARIMA (auto.arima con seasonal = FALSE) ------
modelo_IPC_Arima <- auto.arima(train_IPC_est, seasonal = FALSE)
summary(modelo_IPC_Arima)
checkresiduals(modelo_IPC_Arima)

pred_IPC_Arima <- forecast(modelo_IPC_Arima, h = length(test_IPC))

# Revertir las diferencias con diffinv (dos diferencias no estacionales)
pred_IPC_log_A <- diffinv(pred_IPC_Arima$mean,
                          differences = 2,
                          xi = log(tail(train_IPC, 2)))

# Volver de log a nivel original
pred_IPC_revert_A <- exp(pred_IPC_log_A)

# Eliminar los dos primeros valores (semillas de xi)
pred_IPC_revert_A <- pred_IPC_revert_A[-c(1:2)]

# Comparar con el test real
accuracy_IPC_Arima <- accuracy(pred_IPC_revert_A, test_IPC)
accuracy_IPC_Arima

# ------ SARIMA (auto.arima con seasonal = TRUE) ------
modelo_IPC_sarima <- auto.arima(train_IPC_est, seasonal = TRUE)
summary(modelo_IPC_sarima)
checkresiduals(modelo_IPC_sarima)

pred_IPC_sarima <- forecast(modelo_IPC_sarima, h = length(test_IPC))

# Revertir con diffinv
pred_IPC_log_S <- diffinv(pred_IPC_sarima$mean,
                          differences = 2,
                          xi = log(tail(train_IPC, 2)))

# Convertir de log a nivel original
pred_IPC_revert_S <- exp(pred_IPC_log_S)

# Eliminar los dos valores iniciales
pred_IPC_revert_S <- pred_IPC_revert_S[-c(1:2)]

# Comparar con el test real
accuracy_IPC_sarima <- accuracy(pred_IPC_revert_S, test_IPC)
accuracy_IPC_sarima

# ------ Modelos Naive y SNaive ------
pred_naive  <- naive(train_IPC,  h = length(test_IPC))
pred_snaive <- snaive(train_IPC, h = length(test_IPC))  # estacional (m=12)

accuracy_naive  <- accuracy(pred_naive$mean,  test_IPC)
accuracy_snaive <- accuracy(pred_snaive$mean, test_IPC)

# =================== Gráficos e interpretación ===================

# Data frames para graficar
Meses <- time(test_IPC)
df_test         <- data.frame(Mes = Meses, IPC = as.numeric(test_IPC))
df_pred_arima   <- data.frame(Mes = Meses, Pred = as.numeric(pred_IPC_revert_A))
df_pred_sarima  <- data.frame(Mes = Meses, Pred = as.numeric(pred_IPC_revert_S))
df_naive        <- data.frame(Mes = Meses, Pred = as.numeric(pred_naive$mean))
df_snaive       <- data.frame(Mes = Meses, Pred = as.numeric(pred_snaive$mean))

# ARIMA vs Real
plot_arima <- ggplot() +
  geom_line(data = df_test, aes(x = Mes, y = IPC), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_arima, aes(x = Mes, y = Pred), color = 'red', linetype = 'dashed', size = 1.1) +
  labs(title = 'Pronóstico ARIMA vs IPC real', y = 'IPC', x = 'Mes') +
  theme_minimal()

# SARIMA vs Real
plot_sarima <- ggplot() +
  geom_line(data = df_test, aes(x = Mes, y = IPC), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_sarima, aes(x = Mes, y = Pred), color = 'green', linetype = 'dashed', size = 1.1) +
  labs(title = 'Pronóstico SARIMA vs IPC real', y = 'IPC', x = 'Mes') +
  theme_minimal()

# Mostrar ambos gráficos
grid.arrange(plot_arima, plot_sarima, ncol = 1)

# Todos los modelos juntos
plot_modelos <- ggplot() +
  geom_line(data = df_test, aes(x = Mes, y = IPC), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_arima,  aes(x = Mes, y = Pred), color = 'red',    linetype = 'dashed', size = 1.1) +
  geom_line(data = df_pred_sarima, aes(x = Mes, y = Pred), color = 'green',  linetype = 'dashed', size = 1.1) +
  geom_line(data = df_naive,       aes(x = Mes, y = Pred), color = 'purple', linetype = 'dotted', size = 1.1) +
  geom_line(data = df_snaive,      aes(x = Mes, y = Pred), color = 'orange', linetype = 'dotted', size = 1.1) +
  labs(title = 'Pronóstico IPC: ARIMA, SARIMA, Naive y SNaive vs Real', y = 'IPC', x = 'Mes') +
  theme_minimal()

plot_modelos

## =================== PREDICCIÓN FUTURA Q4 2022 (SIN TEST) ===================

# Entrenamos con todo hasta sep-2022 y predecimos 3 meses (oct-nov-dic)
train_full <- window(IPC_sinO, end = c(2022, 9))

# Transformación como en tu compi
train_full_est <- diff(log(train_full), differences = 2)

# ---- Modelos ----
set.seed(123)
mod_arima  <- auto.arima(train_full_est, seasonal = FALSE)
mod_sarima <- auto.arima(train_full_est, seasonal = TRUE)

summary(mod_arima);  checkresiduals(mod_arima)
summary(mod_sarima); checkresiduals(mod_sarima)

# ---- Predicciones en la serie transformada ----
h <- 3  # Oct, Nov, Dic
fc_arima_t  <- forecast(mod_arima,  h = h)
fc_sarima_t <- forecast(mod_sarima, h = h)

# ---- Función para revertir (diffinv + exp) también para bandas ----
revert_pred <- function(fc_obj, tail_train_log, differences = 2) {
  # mean
  mu  <- diffinv(fc_obj$mean,  differences = differences, xi = tail_train_log)
  # bandas 80%
  lo80 <- diffinv(fc_obj$lower[, "80%"], differences = differences, xi = tail_train_log)
  up80 <- diffinv(fc_obj$upper[, "80%"], differences = differences, xi = tail_train_log)
  # bandas 95%
  lo95 <- diffinv(fc_obj$lower[, "95%"], differences = differences, xi = tail_train_log)
  up95 <- diffinv(fc_obj$upper[, "95%"], differences = differences, xi = tail_train_log)
  
  # quitar los 'differences' primeros (semillas xi) y volver a nivel
  dropn <- differences
  out <- list(
    mean = exp(mu[-seq_len(dropn)]),
    lo80 = exp(lo80[-seq_len(dropn)]),
    up80 = exp(up80[-seq_len(dropn)]),
    lo95 = exp(lo95[-seq_len(dropn)]),
    up95 = exp(up95[-seq_len(dropn)])
  )
  return(out)
}

# Reversión a escala original (usamos las dos últimas observaciones log como xi)
tail_log <- log(tail(train_full, 2))
rev_arima  <- revert_pred(fc_arima_t,  tail_log, differences = 2)
rev_sarima <- revert_pred(fc_sarima_t, tail_log, differences = 2)

# ====== Data para gráficos ======
# Histórico reciente (p.ej., últimos 36 meses para que no quede muy largo)
hist_win <- max(start(IPC_sinO)[1], 2019)
IPC_hist <- window(IPC_sinO, start = c(hist_win, 1), end = c(2022, 9))

df_hist <- data.frame(
  Fecha = time(IPC_hist),
  Valor = as.numeric(IPC_hist)
)

df_pred_arima <- data.frame(
  Fecha = time(ts(rev_arima$mean, start = c(2022,10), frequency = 12)),
  Pred  = as.numeric(rev_arima$mean),
  LI80  = as.numeric(rev_arima$lo80),
  LS80  = as.numeric(rev_arima$up80)
)

df_pred_sarima <- data.frame(
  Fecha = time(ts(rev_sarima$mean, start = c(2022,10), frequency = 12)),
  Pred  = as.numeric(rev_sarima$mean),
  LI80  = as.numeric(rev_sarima$lo80),
  LS80  = as.numeric(rev_sarima$up80)
)

# ====== Gráficos ======

# ARIMA futuro
plot_future_arima <- ggplot() +
  geom_line(data = df_hist, aes(x = Fecha, y = Valor), color = "blue", size = 1.2) +
  geom_ribbon(data = df_pred_arima, aes(x = Fecha, ymin = LI80, ymax = LS80), alpha = 0.15) +
  geom_line(data = df_pred_arima, aes(x = Fecha, y = Pred), color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "IPC mensual – Predicción ARIMA (Oct–Nov–Dic 2022)",
       x = "Año", y = "IPC") +
  theme_minimal()

# SARIMA futuro
plot_future_sarima <- ggplot() +
  geom_line(data = df_hist, aes(x = Fecha, y = Valor), color = "blue", size = 1.2) +
  geom_ribbon(data = df_pred_sarima, aes(x = Fecha, ymin = LI80, ymax = LS80), alpha = 0.15) +
  geom_line(data = df_pred_sarima, aes(x = Fecha, y = Pred), color = "green4", linetype = "dashed", size = 1.2) +
  labs(title = "IPC mensual – Predicción SARIMA (Oct–Nov–Dic 2022)",
       x = "Año", y = "IPC") +
  theme_minimal()

grid.arrange(plot_future_arima, plot_future_sarima, ncol = 1)

# (Opcional) ambos modelos en el mismo gráfico
plot_future_both <- ggplot() +
  geom_line(data = df_hist, aes(x = Fecha, y = Valor), color = "blue", size = 1.2) +
  geom_line(data = df_pred_arima,  aes(x = Fecha, y = Pred), color = "red",   linetype = "dashed", size = 1.2) +
  geom_line(data = df_pred_sarima, aes(x = Fecha, y = Pred), color = "green4", linetype = "dashed", size = 1.2) +
  labs(title = "IPC mensual – Predicción Oct–Nov–Dic 2022 (ARIMA y SARIMA)",
       x = "Año", y = "IPC") +
  theme_minimal()

plot_future_both

# (Opcional) tabla rápida de valores predichos
pred_tabla_q4 <- data.frame(
  Mes = c("2022-10","2022-11","2022-12"),
  ARIMA = round(df_pred_arima$Pred, 3),
  SARIMA = round(df_pred_sarima$Pred, 3),
  ARIMA_LI80 = round(df_pred_arima$LI80, 3),
  ARIMA_LS80 = round(df_pred_arima$LS80, 3),
  SARIMA_LI80 = round(df_pred_sarima$LI80, 3),
  SARIMA_LS80 = round(df_pred_sarima$LS80, 3)
)
print(pred_tabla_q4)

# ========== 7) ACCURACY (ELEGIR EL MEJOR) ==========
acc_arima  <- accuracy(pred_IPC_revert_A, test_IPC)
acc_sarima <- accuracy(pred_IPC_revert_S, test_IPC)

acc_arima
acc_sarima

# (Opcional) Benchmarks:
acc_naive  <- accuracy(naive(train_IPC,  h = h)$mean,  test_IPC)
acc_snaive <- accuracy(snaive(train_IPC, h = h)$mean, test_IPC)
acc_naive
