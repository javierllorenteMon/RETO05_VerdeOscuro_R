# =====================================================================
# IPC-fusion + Validación Cruzada — Código completo listo para ejecutar
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(forecast)
  library(ggplot2)
  library(tseries)
  library(openxlsx)
})

# -------------------- Datos -----------------
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO_M.rds")
stopifnot(frequency(IPC_sinO) == 12)

# -------------------- Train / Test -----------------
train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2021, 1))
test_IPC  <- window(IPC_sinO, start = c(2021, 2), end = c(2022, 9))
h_test    <- length(test_IPC)

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

# Alternativa: modelar en LOG directamente (sin diferenciar manual)
modelo_IPC_Sarima_alt <- auto.arima(log(train_IPC), seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

summary(modelo_IPC_Arima);  checkresiduals(modelo_IPC_Arima)
summary(modelo_IPC_Sarima); checkresiduals(modelo_IPC_Sarima)
summary(modelo_IPC_Sarima_alt); checkresiduals(modelo_IPC_Sarima_alt)

# 3) Forecast en escala estacionaria (para los modelos en diff)
fc_A_t <- forecast(modelo_IPC_Arima,  h = h_test)
fc_S_t <- forecast(modelo_IPC_Sarima, h = h_test)

# 4) Revertir a escala original
tail_log <- log(tail(train_IPC, 2))
revert_pred <- function(fc_obj, xi_log, d = 2) {
  mu   <- diffinv(fc_obj$mean,          differences = d, xi = xi_log)
  lo80 <- diffinv(fc_obj$lower[,"80%"], differences = d, xi = xi_log)
  up80 <- diffinv(fc_obj$upper[,"80%"], differences = d, xi = xi_log)
  dropn <- d
  list(
    mean = exp(mu[-seq_len(dropn)]),
    lo80 = exp(lo80[-seq_len(dropn)]),
    up80 = exp(up80[-seq_len(dropn)])
  )
}
rev_A <- revert_pred(fc_A_t, tail_log, d = 2)
rev_S <- revert_pred(fc_S_t, tail_log, d = 2)

pred_A <- ts(rev_A$mean, start = start(test_IPC), frequency = 12)
pred_S <- ts(rev_S$mean, start = start(test_IPC), frequency = 12)

# 5) Accuracy en TEST (modelos en diff)
acc_A <- accuracy(pred_A, test_IPC)
acc_S <- accuracy(pred_S, test_IPC)
print(acc_A); print(acc_S)

# 6) Gráfico comparativo (TEST) con leyenda
Meses <- time(test_IPC)
df_test   <- data.frame(Mes = Meses, Valor = as.numeric(test_IPC), Modelo = "Real")
df_pred_A <- data.frame(Mes = Meses, Valor = as.numeric(pred_A),   Modelo = "ARIMA")
df_pred_S <- data.frame(Mes = Meses, Valor = as.numeric(pred_S),   Modelo = "SARIMA")
df_all <- bind_rows(df_test, df_pred_A, df_pred_S)

ggplot(df_all, aes(x = Mes, y = Valor, color = Modelo, linetype = Modelo)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Real"="blue","ARIMA"="red","SARIMA"="green4")) +
  scale_linetype_manual(values = c("Real"="solid","ARIMA"="dashed","SARIMA"="dashed")) +
  labs(title = "Comparación IPC real y modelos — Test 2021-02..2022-09",
       x = "Mes", y = "IPC", color = "Modelo", linetype = "Modelo") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# =====================================================================
# PARTE B) Auto ARIMA con Box-Cox — biasadj ACTIVADO
# =====================================================================
set.seed(123)
lam <- BoxCox.lambda(train_IPC)
mod_BC <- auto.arima(train_IPC,
                     seasonal = TRUE,
                     lambda   = lam,
                     stepwise = FALSE,
                     approximation = FALSE)
summary(mod_BC); checkresiduals(mod_BC)

fc_BC  <- forecast(mod_BC, h = h_test, biasadj = TRUE)  # <-- biasadj
acc_BC <- accuracy(fc_BC, test_IPC); print(acc_BC)

# Gráfico Box-Cox vs Real (TEST)
ggplot() +
  geom_line(aes(x = Meses, y = as.numeric(test_IPC), color = "Real", linetype = "Real"), linewidth = 1.1) +
  geom_line(aes(x = Meses, y = as.numeric(fc_BC$mean), color = "Box-Cox", linetype = "Box-Cox"), linewidth = 1.1) +
  scale_color_manual(values = c("Real" = "blue", "Box-Cox" = "black")) +
  scale_linetype_manual(values = c("Real" = "solid", "Box-Cox" = "dashed")) +
  labs(title = "Auto ARIMA con Box-Cox (biasadj) vs IPC real — Test",
       x = "Mes", y = "IPC", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# -------------------- Resumen de modelos ------------------
lb_pval <- function(m, lag=24){
  k <- length(coef(m))
  Box.test(residuals(m), lag=lag, type="Ljung-Box", fitdf=k)$p.value
}
resumen_modelos <- data.frame(
  Modelo     = c("ARIMA log+diff", "SARIMA log+diff", "Auto Box-Cox"),
  AICc       = c(modelo_IPC_Arima$aicc, modelo_IPC_Sarima$aicc, mod_BC$aicc),
  BIC        = c(modelo_IPC_Arima$bic,  modelo_IPC_Sarima$bic,  mod_BC$bic),
  LjungBox_p = c(lb_pval(modelo_IPC_Arima), lb_pval(modelo_IPC_Sarima), lb_pval(mod_BC)),
  RMSE       = c(acc_A["Test set","RMSE"],  acc_S["Test set","RMSE"],  acc_BC["Test set","RMSE"]),
  MAE        = c(acc_A["Test set","MAE"],   acc_S["Test set","MAE"],   acc_BC["Test set","MAE"]),
  MAPE       = c(acc_A["Test set","MAPE"],  acc_S["Test set","MAPE"],  acc_BC["Test set","MAPE"])
)
print(resumen_modelos)

# =====================================================================
# PARTE C) Predicción futura Q4 2022 (Oct-Nov-Dic) — biasadj en Box-Cox
# =====================================================================
train_full <- window(IPC_sinO, end = c(2022, 9))

# A. log+diff (como en A)
train_full_est <- diff(log(train_full), differences = 2)
mod_A_full <- auto.arima(train_full_est, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
mod_S_full <- auto.arima(train_full_est, seasonal=TRUE,  stepwise=FALSE, approximation=FALSE)
fc_A_q4_t <- forecast(mod_A_full, h=3)
fc_S_q4_t <- forecast(mod_S_full, h=3)
tail_log_full <- log(tail(train_full, 2))
rev_A_q4 <- revert_pred(fc_A_q4_t, tail_log_full, d=2)
rev_S_q4 <- revert_pred(fc_S_q4_t, tail_log_full, d=2)

df_pred_A_q4 <- data.frame(Fecha = time(ts(rev_A_q4$mean, start=c(2022,10), frequency=12)),
                           Pred = as.numeric(rev_A_q4$mean))
df_pred_S_q4 <- data.frame(Fecha = time(ts(rev_S_q4$mean, start=c(2022,10), frequency=12)),
                           Pred = as.numeric(rev_S_q4$mean))

# B. Box-Cox (biasadj ACTIVADO)
lam_full <- BoxCox.lambda(train_full)
mod_BC_full <- auto.arima(train_full,
                          seasonal = TRUE,
                          lambda   = lam_full,
                          stepwise = FALSE,
                          approximation = FALSE)
fc_BC_q4 <- forecast(mod_BC_full, h=3, biasadj = TRUE)  # <-- biasadj
df_pred_BC_q4 <- data.frame(Fecha = time(fc_BC_q4$mean),
                            Pred = as.numeric(fc_BC_q4$mean))

# Histórico y gráfico
IPC_hist <- window(IPC_sinO, start=c(2019,1), end=c(2022,9))
df_hist <- data.frame(Fecha=time(IPC_hist), Valor=as.numeric(IPC_hist))

ggplot() +
  geom_line(data = df_hist, aes(Fecha, Valor, color = "Histórico"), linewidth = 1.2) +
  geom_line(data = df_pred_A_q4,  aes(Fecha, Pred, color = "ARIMA"),   linewidth = 1.2, linetype = "dashed") +
  geom_line(data = df_pred_S_q4,  aes(Fecha, Pred, color = "SARIMA"),  linewidth = 1.2, linetype = "dashed") +
  geom_line(data = df_pred_BC_q4, aes(Fecha, Pred, color = "Box-Cox"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Histórico"="blue","ARIMA"="red","SARIMA"="green4","Box-Cox"="black")) +
  labs(title = "Predicción IPC — Oct–Nov–Dic 2022 (ARIMA, SARIMA y Box-Cox)",
       x = "Año", y = "IPC", color = "Serie") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank())

# =====================================================================
# COMPARACIÓN FINAL DE ACCURACY Y MODELO GANADOR
# =====================================================================
acc_arima   <- accuracy(pred_A, test_IPC)
acc_sarima  <- accuracy(pred_S, test_IPC)
acc_boxcox  <- accuracy(fc_BC$mean, test_IPC)

rmse_vals <- c("ARIMA" = acc_arima["Test set","RMSE"],
               "SARIMA" = acc_sarima["Test set","RMSE"],
               "Box-Cox" = acc_boxcox["Test set","RMSE"])
best_model <- names(which.min(rmse_vals))
best_rmse  <- min(rmse_vals)

cat("\n#-------------------------------------------------------------------------------\n")
cat("# Mejor modelo según RMSE en TEST:", best_model, "\n")
cat("# RMSE:", round(best_rmse, 4), "\n")
cat("#-------------------------------------------------------------------------------\n")

# =====================================================================
# DETALLE DEL MEJOR MODELO (p,d,q)(P,D,Q)[m]
# =====================================================================
if (best_model == "SARIMA") {
  orden <- modelo_IPC_Sarima$arma
  cat("# Mejor modelo SARIMA: ARIMA(", orden[1], ",", orden[6], ",", orden[2], ")(",
      orden[3], ",", orden[7], ",", orden[4], ")[", orden[5], "] — (mejor accuracy)\n", sep="")
}
if (best_model == "ARIMA") {
  orden <- modelo_IPC_Arima$arma
  cat("# Mejor modelo ARIMA: ARIMA(", orden[1], ",", orden[6], ",", orden[2], ")(",
      orden[3], ",", orden[7], ",", orden[4], ")[", orden[5], "] — (mejor accuracy)\n", sep="")
}
if (best_model == "Box-Cox") {
  orden <- mod_BC$arma
  cat("# Mejor modelo Box-Cox: ARIMA(", orden[1], ",", orden[6], ",", orden[2], ")(",
      orden[3], ",", orden[7], ",", orden[4], ")[", orden[5], "] — (mejor accuracy)\n", sep="")
}

# =====================================================================
# VALIDACIÓN CRUZADA (rolling-origin) — Comparación de 5 modelos
# =====================================================================

# Helpers CV
rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mase <- function(e, y, m = frequency(y)){
  scale <- mean(abs(diff(y, lag = m)), na.rm = TRUE)
  mean(abs(e), na.rm = TRUE) / scale
}
revert_pred_vec <- function(mean_t, xi_log, d = 2){
  mu <- diffinv(mean_t, differences = d, xi = xi_log)
  as.numeric(exp(mu[-seq_len(d)]))
}
# Wrappers (reciben y en nivel, devuelven vector en nivel)
f_auto_arima_log <- function(y, h){
  fit <- auto.arima(log(y), seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  fc  <- forecast(fit, h = h, lambda = 0, biasadj = TRUE)
  as.numeric(fc[["mean"]])
}
f_sarima_logdiff <- function(y, h){
  y_est  <- diff(log(y), differences = 2)
  fit    <- auto.arima(y_est, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  fc_t   <- forecast(fit, h = h)
  xi_log <- log(tail(y, 2))
  revert_pred_vec(fc_t[["mean"]], xi_log, d = 2)
}
f_arima_logdiff <- function(y, h){
  y_est  <- diff(log(y), differences = 2)
  fit    <- auto.arima(y_est, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  fc_t   <- forecast(fit, h = h)
  xi_log <- log(tail(y, 2))
  revert_pred_vec(fc_t[["mean"]], xi_log, d = 2)
}
f_auto_boxcox <- function(y, h){
  lam <- BoxCox.lambda(y)
  fit <- auto.arima(y, seasonal = TRUE, lambda = lam, stepwise = FALSE, approximation = FALSE)
  fc  <- forecast(fit, h = h, biasadj = TRUE)
  as.numeric(fc[["mean"]])
}
f_snaive <- function(y, h){
  m <- frequency(y); last_season <- tail(y, m)
  rep(last_season, length.out = h)
}
# CV rolling-origin (error del paso h)
tscv_h <- function(y, f, h, warm = max(24, 2*frequency(y))){
  n <- length(y); err <- rep(NA_real_, n)
  for (i in seq.int(warm, n - h)){
    yi <- y[1:i]
    fc <- f(yi, h)
    err[i + h] <- yi[(i + h)] - fc[h]
  }
  ts(err, start = start(y), frequency = frequency(y))
}

set.seed(123)
e1_auto_log   <- tscv_h(train_IPC, f_auto_arima_log, h = 1)
e1_sar_ldiff  <- tscv_h(train_IPC, f_sarima_logdiff, h = 1)
e1_ar_ldiff   <- tscv_h(train_IPC, f_arima_logdiff,  h = 1)
e1_boxcox     <- tscv_h(train_IPC, f_auto_boxcox,   h = 1)
e1_snaive     <- tscv_h(train_IPC, f_snaive,        h = 1)

e3_auto_log   <- tscv_h(train_IPC, f_auto_arima_log, h = 3)
e3_sar_ldiff  <- tscv_h(train_IPC, f_sarima_logdiff, h = 3)
e3_ar_ldiff   <- tscv_h(train_IPC, f_arima_logdiff,  h = 3)
e3_boxcox     <- tscv_h(train_IPC, f_auto_boxcox,   h = 3)
e3_snaive     <- tscv_h(train_IPC, f_snaive,        h = 3)

res_cv <- data.frame(
  Modelo  = c("auto.arima(log)", "SARIMA log+diff", "ARIMA log+diff", "Auto Box-Cox", "SNaive"),
  RMSE_h1 = c(rmse(e1_auto_log), rmse(e1_sar_ldiff), rmse(e1_ar_ldiff), rmse(e1_boxcox), rmse(e1_snaive)),
  MASE_h1 = c(mase(e1_auto_log, train_IPC), mase(e1_sar_ldiff, train_IPC), mase(e1_ar_ldiff, train_IPC),
              mase(e1_boxcox, train_IPC), mase(e1_snaive, train_IPC)),
  RMSE_h3 = c(rmse(e3_auto_log), rmse(e3_sar_ldiff), rmse(e3_ar_ldiff), rmse(e3_boxcox), rmse(e3_snaive)),
  MASE_h3 = c(mase(e3_auto_log, train_IPC), mase(e3_sar_ldiff, train_IPC), mase(e3_ar_ldiff, train_IPC),
              mase(e3_boxcox, train_IPC), mase(e3_snaive, train_IPC))
)
res_cv <- res_cv[order(res_cv$RMSE_h1, res_cv$RMSE_h3), ]
print(res_cv, digits = 4)

# Reajustar ganador por CV y evaluar en TEST
fit_and_forecast <- function(name, y_train, h){
  switch(name,
         "auto.arima(log)" = {
           fit <- auto.arima(log(y_train), seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
           forecast(fit, h = h, lambda = 0, biasadj = TRUE)[["mean"]]
         },
         "SARIMA log+diff" = {
           y_est  <- diff(log(y_train), differences = 2)
           fit    <- auto.arima(y_est, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
           xi_log <- log(tail(y_train, 2))
           revert_pred_vec(forecast(fit, h = h)[["mean"]], xi_log, d = 2)
         },
         "ARIMA log+diff" = {
           y_est  <- diff(log(y_train), differences = 2)
           fit    <- auto.arima(y_est, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
           xi_log <- log(tail(y_train, 2))
           revert_pred_vec(forecast(fit, h = h)[["mean"]], xi_log, d = 2)
         },
         "Auto Box-Cox" = {
           lam <- BoxCox.lambda(y_train)
           fit <- auto.arima(y_train, seasonal = TRUE, lambda = lam, stepwise = FALSE, approximation = FALSE)
           forecast(fit, h = h, biasadj = TRUE)[["mean"]]
         },
         "SNaive" = {
           m <- frequency(y_train); last_season <- tail(y_train, m)
           rep(last_season, length.out = h)
         })
}
best_name <- res_cv$Modelo[1]
cat(">> Mejor por CV:", best_name, "\n")

pred_cv_best <- ts(as.numeric(fit_and_forecast(best_name, train_IPC, length(test_IPC))),
                   start = start(test_IPC), frequency = frequency(test_IPC))
print(accuracy(pred_cv_best, test_IPC))

# Gráfico comparativo Real vs ganador por CV
df_plot <- bind_rows(
  data.frame(Mes = Meses, Valor = as.numeric(test_IPC),       Serie = "Real"),
  data.frame(Mes = Meses, Valor = as.numeric(pred_cv_best),   Serie = best_name)
)
ggplot(df_plot, aes(Mes, Valor, color = Serie, linetype = Serie)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Real" = "blue", best_name = "green4")) +
  scale_linetype_manual(values = c("Real" = "solid", best_name = "dashed")) +
  labs(title = paste0("IPC real vs ", best_name, " — Test (selección por CV)"),
       x = "Mes", y = "IPC", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
