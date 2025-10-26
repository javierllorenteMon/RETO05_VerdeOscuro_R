### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL IPC

# Cargar librerias
library(zoo)
library(car)
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)
options(scipen=999)
source("Scripts_Preprocesamiento/Funciones.R")

IPC_sinO   <- readRDS("Datos/transformados/IPC_sinO_M.rds")
MS_sinO   <- readRDS("Datos/transformados/MS_sinO_M.rds")
SMI_sinO   <- readRDS("Datos/transformados/SMI_sinO_M.rds")
UR_sinO   <- readRDS("Datos/transformados/UR_sinO_M.rds")


train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2022, 1))
test_IPC  <- window(IPC_sinO, start = c(2022, 2),  end = c(2022, 9))

train_MS  <- window(MS_sinO,  start = start(train_IPC), end = end(train_IPC))
test_MS   <- window(MS_sinO,  start = start(test_IPC),  end = end(test_IPC))

train_SMI <- window(SMI_sinO, start = start(train_IPC), end = end(train_IPC))
test_SMI  <- window(SMI_sinO, start = start(test_IPC),  end = end(test_IPC))

train_UR  <- window(UR_sinO,  start = start(train_IPC), end = end(train_IPC))
test_UR   <- window(UR_sinO,  start = start(test_IPC),  end = end(test_IPC))

autoplot(IPC_sinO) + ggtitle("IPC – serie completa")


# Descomposición de las series para ver tendencia, estacionalidad y residuales
decomIPC <- decompose(IPC_sinO)
autoplot(decomIPC)

decomMS <- decompose(MS_sinO)
autoplot(decomMS)

decomUR <- decompose(UR_sinO)
autoplot(decomUR)

decomSMI <- decompose(SMI_sinO)
autoplot(decomSMI)


# 2. Análisis exploratorio y estacionariedad

# Aplicar log si la serie tiene varianza creciente

# Comprobar varianza creciente
ts.plot(train_IPC) # si parece tener varianza creciente
train_IPC_log <- log(train_IPC)

# Comprobar estacionalidad
nsdiffs(train_IPC)
acf(train_IPC)

# Diferencias para estacionarizar
train_IPC_est <-  diff( diff(log(train_IPC), lag = 12), differences = 1)

# Comprobar estacionariedad
test_estacionariedad(train_IPC_est)

# -----------------------------
# Variables exógenas
# -----------------------------

# Transformaciones: log si varianza creciente y diferencias si tendencia

# Comprobar varianza creciente
ts.plot(train_MS) # no
ts.plot(train_UR) # no
ts.plot(train_SMI) # no

# Comprobar estacionlidad
acf(train_MS)
acf(train_UR)
acf(train_SMI)

# Diferencias para estacionar y quitar tendencias
train_MS_est <- diff(train_MS, differences = 2)
train_UR_est <- diff(train_UR, differences = 2)
train_SMI_est <- diff(train_SMI, differences = 2)

# Comprobar estacionariedad
test_estacionariedad(train_MS_est)
test_estacionariedad(train_UR_est)
test_estacionariedad(train_SMI_est)

# Test exógenas: aplicar las mismas transformaciones que en train
test_MS_est <- diff(test_MS, differences = 2)
test_UR_est <- diff(test_UR, differences = 2)
test_SMI_est <- diff(test_SMI, differences = 2)

# Combinar en matrices
X_train <- cbind(MS = train_MS, UR = train_UR, SMI = train_SMI)
X_test <- cbind(MS = test_MS, UR = test_UR, SMI = test_SMI)

X_train_est <- cbind(MS = train_MS_est, UR = train_UR_est, SMI = train_SMI_est)
X_test_est <- cbind(MS = test_MS_est, UR = test_UR_est, SMI = test_SMI_est)

#Box-Cox

lambda <- BoxCox.lambda(train_IPC)           # si ~0 ⇒ log
x_bc   <- BoxCox(train_IPC, lambda)
paste("lambda =", round(lambda, 3))
autoplot(x_bc) + ggtitle(sprintf("Train transformado (λ=%.3f)", lambda))


# ------ Modelos ------

# ARIMA sin estacionalidad (baseline)
fit_arima1 <- auto.arima(train_IPC, seasonal = FALSE,
                         lambda = lambda, stepwise = FALSE, approximation = FALSE)

#Arima Estacionaria manual
fit_arimaEst <- arima(train_IPC_est,
                      order    = c(2, 0, 2),     # d=0 porque ya está diferenciada
                      seasonal = list(order=c(1,0,0), period=12),     # D=0 porque ya está diferenciada estacionalmente
                      include.mean = FALSE)

# ARIMA con estacionalidad permitida 
fit_arima2 <- auto.arima(train_IPC, seasonal = FALSE, 
                         lambda = lambda, stepwise = FALSE, approximation = FALSE,
                         allowdrift = FALSE)

# SARIMA "forzado" a considerar componente estacional si la hay
fit_sarima <- auto.arima(train_IPC, seasonal = TRUE,
                         lambda = lambda, stepwise = FALSE, approximation = FALSE)

checkresiduals(fit_arima1)
checkresiduals(fit_arimaEst)
checkresiduals(fit_arima2)
checkresiduals(fit_sarima)

# Pronósticos en escala original (forecast revierte Box-Cox al llevar lambda dentro)
h <- length(test_IPC)


#Revertir arima manual
fc_est <- forecast(fit_arimaEst, h = h)$mean  # pronósticos en escala ∇∇_12 log
x_log <- log(train_IPC)

# Inversión: primero la no estacional
y_seas_diff <- diff(x_log, lag = 12)
y_d1 <- diffinv(fc_est, xi = tail(y_seas_diff, 1))[-1]

# Luego la estacional (lag 12) usando las 12 últimas observaciones de log
y_lvl_log <- diffinv(y_d1, lag = 12, xi = tail(x_log, 12))[-(1:12)]

# Volver a escala original
fc_arimaEst_lvl <- ts(
  exp(y_lvl_log),
  start     = tsp(train_IPC)[2] + 1/frequency(train_IPC),
  frequency = frequency(train_IPC)
)

fc_arima1 <- forecast(fit_arima1, h = h)
fc_arima2 <- forecast(fit_arima2, h = h)
fc_sarima <- forecast(fit_sarima, h = h)

fc_list <- list(
  `ARIMA1` = forecast(fit_arima1, h=h),
  `ARIMAEst` = structure(list(mean = fc_arimaEst_lvl), class = "forecast"),
  `ARIMA2` = forecast(fit_arima2, h=h),
  `SARIMA` = forecast(fit_sarima, h=h)
)

# Comparación rápida contra test
autoplot(fc_arima1) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA1 vs Test")
autoplot(fc_arimaEst_lvl) + autolayer(test_IPC, series="Test") + ggtitle("ARIMAEst vs Test")
autoplot(fc_arima2) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA2 vs Test")
autoplot(fc_sarima) + autolayer(test_IPC, series="Test") + ggtitle("SARIMA vs Test")

accuracy(fc_arima1$mean, test_IPC)
accuracy(fc_arimaEst_lvl, test_IPC)
accuracy(fc_arima2$mean, test_IPC)
accuracy(fc_sarima$mean, test_IPC)

# ------ Metricas + LjungBox ------
safe_lb <- function(model, lag = 24, fitdf = NULL) {
  res <- try({
    if (is.null(fitdf)) fitdf <- length(coef(model))
    Box.test(residuals(model), lag = lag, type = "Ljung-Box", fitdf = fitdf)$p.value
  }, silent = TRUE)
  if (inherits(res, "try-error")) NA_real_ else as.numeric(res)
}
lb_map <- c(
  `ARIMA1` = safe_lb(fit_arima1, lag=24),
  `ARIMAEst` = safe_lb(fit_arimaEst, lag=24),
  `ARIMA2` = safe_lb(fit_arima2, lag=24),
  `SARIMA` = safe_lb(fit_sarima, lag=24)
)

acc_tab <- do.call(rbind, lapply(names(fc_list), function(nm){
  acc <- accuracy(fc_list[[nm]], test_IPC)["Test set", c("ME","RMSE","MAE","MAPE")]
  data.frame(Modelo=nm, t(acc), row.names=NULL, check.names=FALSE)
})) %>%
  mutate(LjungBox_p = as.numeric(lb_map[Modelo])) %>%
  arrange(RMSE)

cat("\n# === Comparativa en TEST (ordenado por RMSE) ===\n")
print(acc_tab, row.names=FALSE)

# ------ Seleccionar ganador ------
valid <- acc_tab %>% filter(!is.na(LjungBox_p) & LjungBox_p >= 0.05)
winner_name <- if (nrow(valid) > 0) {
  valid$Modelo[which.min(valid$RMSE)]
} else {
  acc_tab$Modelo[which.min(acc_tab$RMSE)]
}

lb_winner <- (acc_tab %>% filter(Modelo == winner_name))$LjungBox_p
cat("\nGanador provisional:", winner_name,
    ifelse(is.na(lb_winner) | lb_winner < 0.05,
           " (no pasa Ljung-Box; elegido por RMSE)", ""), "\n")

# Mantenemos el mapa de modelos por si lo quieres usar después
fit_map <- list(`ARIMA1`=fit_arima1, `ARIMA2`=fit_arima2, `SARIMA`=fit_sarima)
WIN_FIT <- fit_map[[winner_name]]           
WIN_FC  <- fc_list[[winner_name]]       

# ------  Métricas claras + tabla de valores TEST ------
h <- length(test_IPC)
fechas_test <- as.yearmon(time(test_IPC))

pred_test_tbl <- data.frame(
  Fecha_YM       = format(fechas_test, "%Y-%m"),
  Observado      = round(as.numeric(test_IPC), 3),
  `Pred ganador` = round(as.numeric(WIN_FC$mean[seq_len(h)]), 3),
  check.names    = FALSE
)

cat("\n# === Tabla de predicciones en TEST (ganador) ===\n")
print(pred_test_tbl, row.names = FALSE)

cat("\nGanador definitivo:", winner_name,
    "| RMSE test =", round((acc_tab %>% filter(Modelo==winner_name))$RMSE, 3),
    "| Ljung-Box p =", round(lb_winner, 4), "\n")


# =========================================================
# GANADOR PROVISIONAL ARIMA(1,1,3)(1,1,0)[12]
# =========================================================
ggtsdisplay(residuals(WIN_FC))
checkresiduals(WIN_FC)


# ===============      ARIMAX (exógenas)     ==============

# --- (2) Crear ventanas train/test para exógenas ---
train_MS  <- window(MS_sinO,  start = start(train_IPC), end = end(train_IPC))
test_MS   <- window(MS_sinO,  start = start(test_IPC),  end = end(test_IPC))

train_SMI <- window(SMI_sinO, start = start(train_IPC), end = end(train_IPC))
test_SMI  <- window(SMI_sinO, start = start(test_IPC),  end = end(test_IPC))

train_UR  <- window(UR_sinO,  start = start(train_IPC), end = end(train_IPC))
test_UR   <- window(UR_sinO,  start = start(test_IPC),  end = end(test_IPC))

# --- (3) Limpieza básica de NA + chequeos de longitudes ---
fix_na <- function(v) if (anyNA(v)) forecast::na.interp(v) else v

train_MS  <- fix_na(train_MS);  test_MS  <- fix_na(test_MS)
train_SMI <- fix_na(train_SMI); test_SMI <- fix_na(test_SMI)
train_UR  <- fix_na(train_UR);  test_UR  <- fix_na(test_UR)

# --- (4) Estandarizar exógenas con parámetros del TRAIN ---
std_fit <- function(...) {
  X <- cbind(...)
  mu <- colMeans(X, na.rm=TRUE)
  sdv <- apply(X, 2, sd, na.rm=TRUE)
  Xs <- scale(X, center = mu, scale = sdv)
  list(mu=mu, sdv=sdv, Xstd=as.matrix(Xs))
}
std_apply <- function(X, mu, sdv) {
  as.matrix(scale(cbind(X), center = mu, scale = sdv))
}

S_MS   <- std_fit(MS = as.numeric(train_MS))
S_MSS  <- std_fit(MS = as.numeric(train_MS),
                  SMI= as.numeric(train_SMI))
S_MSSU <- std_fit(MS = as.numeric(train_MS),
                  SMI= as.numeric(train_SMI),
                  UR = as.numeric(train_UR))

Xtr_MS   <- S_MS$Xstd
Xtr_MSS  <- S_MSS$Xstd
Xtr_MSSU <- S_MSSU$Xstd

Xte_MS   <- std_apply(as.numeric(test_MS),S_MS$mu,  S_MS$sdv)
Xte_MSS  <- std_apply(cbind(as.numeric(test_MS),as.numeric(test_SMI)),S_MSS$mu,  S_MSS$sdv)
Xte_MSSU <- std_apply(cbind(as.numeric(test_MS),as.numeric(test_SMI),
                            as.numeric(test_UR)),S_MSSU$mu, S_MSSU$sdv)



# Mirar correlacion
datos_tr <- data.frame(
  Fecha = as.yearmon(time(train_IPC)),
  IPC   = as.numeric(train_IPC),
  MS    = as.numeric(train_MS),
  SMI   = as.numeric(train_SMI),
  UR    = as.numeric(train_UR)
)
datos_tr <- na.omit(datos_tr)

fit_all <- lm(IPC ~ MS + SMI + UR, data = datos_tr)
R_multi <- sqrt(summary(fit_all)$r.squared)
R_multi

vif(fit_all)

fit_MS     <- lm(IPC ~ MS, data = datos_tr)
fit_MS_SMI <- lm(IPC ~ MS + SMI, data = datos_tr)
fit_MS_SMI_UR <- lm(IPC ~ MS + SMI + UR, data = datos_tr)

c(
  R2_MS          = summary(fit_MS)$r.squared,
  R2_MS_SMI      = summary(fit_MS_SMI)$r.squared,
  dR2_add_SMI    = summary(fit_MS_SMI)$r.squared - summary(fit_MS)$r.squared,
  R2_MS_SMI_UR   = summary(fit_MS_SMI_UR)$r.squared,
  dR2_add_UR     = summary(fit_MS_SMI_UR)$r.squared - summary(fit_MS_SMI)$r.squared
)

# las mejores combinaciones probadas: MS, MS+SMI, MS+SMI+UR
# --- (5) Ajuste ARIMAX 
ARIMAX_MS <- auto.arima(train_IPC, xreg = Xtr_MS,
                        lambda=lambda, biasadj=TRUE,
                        seasonal=TRUE,
                        stepwise=FALSE, approximation=FALSE)

ARIMAX_MSS <- auto.arima(train_IPC, xreg = Xtr_MSS,
                         lambda=lambda, biasadj=TRUE,
                         seasonal=TRUE, 
                         stepwise=FALSE, approximation=FALSE)

ARIMAX_MSSU <- auto.arima(train_IPC, xreg = Xtr_MSSU,
                          lambda=lambda, biasadj=TRUE,
                          seasonal=TRUE, 
                          stepwise=FALSE, approximation=FALSE)

cat("\n[Modelos ARIMAX]\n")
print(ARIMAX_MS)
print(ARIMAX_MSS)
print(ARIMAX_MSSU)

# --- (6) Pronósticos en TEST (h = |test|) ---
H <- length(test_IPC)
fc_MS   <- forecast(ARIMAX_MS,   xreg = Xte_MS,   h = H)
fc_MSS  <- forecast(ARIMAX_MSS,  xreg = Xte_MSS,  h = H)
fc_MSSU <- forecast(ARIMAX_MSSU, xreg = Xte_MSSU, h = H)

# --- (7) Accuracy + Ljung–Box ---
safe_lb <- function(fit, lag=24){
  tryCatch(Box.test(residuals(fit), lag=lag, type="Ljung-Box",
                    fitdf=length(coef(fit)))$p.value,
           error=function(e) NA_real_)
}
acc_row <- function(nm, fc){
  out <- accuracy(fc, test_IPC)["Test set", c("ME","RMSE","MAE","MAPE")]
  data.frame(Modelo = nm, t(out), check.names = FALSE, row.names = NULL)
}

ACC <- rbind(
  acc_row("ARIMAX_MS",        fc_MS),
  acc_row("ARIMAX_MS+SMI",    fc_MSS),
  acc_row("ARIMAX_MS+SMI+UR", fc_MSSU)
)
ACC$LjungBox_p <- c(safe_lb(ARIMAX_MS), safe_lb(ARIMAX_MSS), safe_lb(ARIMAX_MSSU))
ACC <- ACC[order(ACC$RMSE), ]

cat("\n# === Comparativa ARIMAX en TEST (ordenado por RMSE) ===\n")
print(ACC, row.names=FALSE)

# --- (8) Elegir ganador (si no pasa LB, aún así por RMSE) ---
valid <- ACC[!is.na(ACC$LjungBox_p) & ACC$LjungBox_p >= 0.05, ]
if (nrow(valid) > 0) {
  winner <- valid$Modelo[ which.min(valid$RMSE) ]
} else {
  winner <- ACC$Modelo[1]
}
cat("\nGanador:", winner,
    "| RMSE test =", round((ACC %>% dplyr::filter(Modelo==winner))$RMSE,3),
    "| Ljung-Box p =", round((ACC %>% dplyr::filter(Modelo==winner))$LjungBox_p,4), "\n")

# --- (9) Tabla Pred vs Obs del ganador ---
FC_WIN <- switch(winner,
                 "ARIMAX_MS"          = fc_MS,
                 "ARIMAX_MS+SMI"      = fc_MSS,
                 "ARIMAX_MS+SMI+UR"   = fc_MSSU)

fechas_test <- format(as.yearmon(time(test_IPC)), "%Y-%m")
pred_tbl <- data.frame(
  Fecha_YM    = fechas_test,
  Observado   = round(as.numeric(test_IPC), 3),
  Pred_ARIMAX = round(as.numeric(FC_WIN$mean), 3),
  check.names = FALSE
)
cat("\n# === Predicciones en TEST (ganador) ===\n")
print(pred_tbl, row.names = FALSE)

# --- (10) Diagnóstico de residuos del ganador ---
WIN_FIT <- switch(winner,
                  "ARIMAX_MS"          = ARIMAX_MS,
                  "ARIMAX_MS+SMI"      = ARIMAX_MSS,
                  "ARIMAX_MS+SMI+UR"   = ARIMAX_MSSU)
ggtsdisplay(residuals(WIN_FIT))
checkresiduals(WIN_FIT)

#TABLA COMPARACIONES TODOS LOS MODELOS TEST
fc_arima_test  <- fc_list[["ARIMA1"]]
fc_arimaEst_test  <- fc_list[["ARIMAEst"]]
fc_arima2_test <- fc_list[["ARIMA2"]]
fc_sarima_test <- fc_list[["SARIMA"]]

idx <- as.Date(as.yearmon(time(test_IPC)))
n <- length(test_IPC)  
pred_test_tbl <- data.frame(
  Anio      = as.integer(format(idx, "%Y")),
  Mes       = as.integer(format(idx, "%m")),
  Fecha_YM  = format(idx, "%Y-%m"),
  Observado = round(as.numeric(test_IPC), 3),
  ARIMA1    = round(as.numeric(fc_arima_test$mean)[seq_len(n)], 3),
  ARIMAEst  = round(as.numeric(fc_arimaEst_test$mean)[seq_len(n)], 3),
  ARIMA2    = round(as.numeric(fc_arima2_test$mean)[seq_len(n)], 3),
  SARIMA    = round(as.numeric(fc_sarima_test$mean)[seq_len(n)], 3),
  ARIMAX    = round(as.numeric(fc_MS$mean)[seq_len(n)], 3),
  check.names = FALSE
)
print(pred_test_tbl, row.names = FALSE)

################################################################################
# 7. CROSS-VALIDATION (Rolling forecast) con los modelos de IPC (h=1)
################################################################################
# 
# # --- Parámetros de CV ---
# h <- 1                                     # horizonte 1 mes
# n <- length(IPC_sinO)                      # longitud total
# train_size <- max(84, 2*frequency(IPC_sinO) + 24)  # ventana inicial razonable (>=7 años)
# cat("Iniciando CV. Tamaño inicial del train:", train_size, "meses.\n")
# 
# # Lambda fijo para todo el CV (coherente con tu pipeline)
# lambda_cv <- BoxCox.lambda(train_IPC)
# 
# # Vectores de predicción (h=1) para cada modelo
# m <- n - train_size
# pred_arima1_cv      <- rep(NA_real_, m)
# pred_sarima_cv      <- rep(NA_real_, m)
# pred_arimaEst_cv    <- rep(NA_real_, m)
# pred_arimax_MSSU_cv <- rep(NA_real_, m)
# 
# # Utilidades
# fix_na <- function(v) if (anyNA(v)) forecast::na.interp(v) else v
# 
# # Barra de progreso
# pb <- txtProgressBar(min = 0, max = m, style = 3, width = 50, char = "=")
# k <- 0
# 
# cat("Iniciando Cross-Validation (Rolling Forecast)...\n")
# for (i in train_size:(n-1)) {
#   # Índice donde guardamos la predicción de esta ventana
#   idx_pred <- i - train_size + 1
#   
#   # ---------- Ventana de entrenamiento (target) ----------
#   train_IPC_cv <- window(IPC_sinO, end = time(IPC_sinO)[i])
#   
#   # ---------- Ventanas exógenas (alineadas) ----------
#   train_MS_cv  <- window(MS_sinO,  end = time(MS_sinO)[i])
#   train_SMI_cv <- window(SMI_sinO, end = time(SMI_sinO)[i])
#   train_UR_cv  <- window(UR_sinO,  end = time(UR_sinO)[i])
#   
#   # Exógenas para el siguiente paso (t+1)
#   if (i+1 <= length(MS_sinO)) {
#     next_MS  <- MS_sinO[i+1]
#     next_SMI <- SMI_sinO[i+1]
#     next_UR  <- UR_sinO[i+1]
#   } else {
#     next_MS <- next_SMI <- next_UR <- NA_real_
#   }
#   
#   # ---------- Modelo 1: ARIMA1 (auto, no estacional, lambda) ----------
#   fit_arima1_cv <- try(
#     auto.arima(train_IPC_cv, seasonal = FALSE, lambda = lambda_cv,
#                stepwise = TRUE, approximation = TRUE),
#     silent = TRUE
#   )
#   if (!inherits(fit_arima1_cv, "try-error")) {
#     pred_arima1_cv[idx_pred] <- as.numeric(forecast(fit_arima1_cv, h = h)$mean)
#   }
#   
#   # ---------- Modelo 2: SARIMA (auto, estacional, lambda) ----------
#   fit_sarima_cv <- try(
#     auto.arima(train_IPC_cv, seasonal = TRUE, lambda = lambda_cv,
#                stepwise = TRUE, approximation = TRUE),
#     silent = TRUE
#   )
#   if (!inherits(fit_sarima_cv, "try-error")) {
#     pred_sarima_cv[idx_pred] <- as.numeric(forecast(fit_sarima_cv, h = h)$mean)
#   }
#   
#   # ---------- Modelo 3: ARIMAEst (manual en LOG, con inversión) ----------
#   # Transformación segura en log
#   x_log_cv <- log(fix_na(train_IPC_cv))
#   # Diferencias: ∇ ∇_12 log y_t
#   y_diff_cv <- diff(diff(x_log_cv, lag = 12), differences = 1)
#   
#   fit_manu_cv <- try(
#     forecast::Arima(y = y_diff_cv,
#                     order = c(2,0,2),
#                     seasonal = list(order = c(1,0,0), period = 12),
#                     include.mean = FALSE),
#     silent = TRUE
#   )
#   if (!inherits(fit_manu_cv, "try-error")) {
#     # Pronóstico en escala diferenciada
#     fc_manu <- forecast(fit_manu_cv, h = h)$mean  # vector de longitud h
#     # Inversión: primero no estacional
#     y_seas_diff_cv <- diff(x_log_cv, lag = 12)
#     y_d1_cv <- diffinv(as.numeric(fc_manu), xi = tail(y_seas_diff_cv, 1))[-1]
#     # Luego estacional (lag=12)
#     y_lvl_log_cv <- diffinv(y_d1_cv, lag = 12, xi = tail(x_log_cv, 12))[-(1:12)]
#     # Volver a escala original
#     pred_arimaEst_cv[idx_pred] <- exp(y_lvl_log_cv)[1]
#   }
#   
#   # ---------- Modelo 4: ARIMAX (MS+SMI+UR), estandarizado por ventana ----------
#   # Estandarizar con parámetros de la ventana
#   Xtr_list <- list(MS = as.numeric(train_MS_cv),
#                    SMI = as.numeric(train_SMI_cv),
#                    UR  = as.numeric(train_UR_cv))
#   mu_cv  <- sapply(Xtr_list, mean, na.rm = TRUE)
#   sdv_cv <- sapply(Xtr_list, sd,   na.rm = TRUE)
#   Xtr_cv <- scale(do.call(cbind, Xtr_list), center = mu_cv, scale = sdv_cv)
#   
#   Xnext_mat <- matrix(c(next_MS, next_SMI, next_UR), nrow = 1)
#   Xte_cv <- scale(Xnext_mat, center = mu_cv, scale = sdv_cv)
#   
#   fit_arimax_cv <- try(
#     auto.arima(train_IPC_cv,
#                xreg = Xtr_cv,
#                lambda = lambda_cv,
#                seasonal = TRUE,
#                stepwise = TRUE, approximation = TRUE),
#     silent = TRUE
#   )
#   if (!inherits(fit_arimax_cv, "try-error") && !any(is.na(Xte_cv))) {
#     pred_arimax_MSSU_cv[idx_pred] <- as.numeric(
#       forecast(fit_arimax_cv, xreg = Xte_cv, h = h, biasadj = TRUE)$mean
#     )
#   }
#   
#   # Progreso
#   k <- k + 1
#   setTxtProgressBar(pb, k)
# }
# close(pb)
# cat("Cross-Validation finalizado.\n")
# 
# # ------------------ Métricas CV ------------------
# # Observado (a partir del primer punto pronosticado)
# actual_cv <- window(IPC_sinO, start = time(IPC_sinO)[train_size + 1])
# 
# # Alinear longitudes por seguridad
# len <- length(actual_cv)
# pred_arima1_cv      <- tail(pred_arima1_cv,      len)
# pred_sarima_cv      <- tail(pred_sarima_cv,      len)
# pred_arimaEst_cv    <- tail(pred_arimaEst_cv,    len)
# pred_arimax_MSSU_cv <- tail(pred_arimax_MSSU_cv, len)
# 
# # Errores
# e1 <- actual_cv - pred_arima1_cv
# e2 <- actual_cv - pred_sarima_cv
# e3 <- actual_cv - pred_arimaEst_cv
# e4 <- actual_cv - pred_arimax_MSSU_cv
# 
# metrics <- function(err, act){
#   c(
#     RMSE = sqrt(mean(err^2, na.rm = TRUE)),
#     MAE  = mean(abs(err), na.rm = TRUE),
#     MAPE = mean(abs(err/act), na.rm = TRUE) * 100
#   )
# }
# 
# tab_cv <- data.frame(
#   Modelo = c("ARIMA1 (auto)", "SARIMA (auto)", "ARIMA manual (log)", "ARIMAX MS+SMI+UR (auto)"),
#   rbind(
#     metrics(e1, actual_cv),
#     metrics(e2, actual_cv),
#     metrics(e3, actual_cv),
#     metrics(e4, actual_cv)
#   ),
#   check.names = FALSE
# )
# print(tab_cv, row.names = FALSE)
# saveRDS(tab_cv, file = "Datos/Resultados/CV_metrics_IPC.rds")
tab_cv <- readRDS("Datos/Resultados/CV_metrics_IPC.rds")
print(tab_cv, row.names = FALSE)

# ================== Pronóstico futuro con ARIMA MANUAL + SARIMA ==================
# 1.1) Preparar datos (usando la serie COMPLETA)
# (Transformación: log -> diff(lag=12) -> diff(lag=1))
ipc_full_log <- log(IPC_sinO)
ipc_full_est <- diff(diff(ipc_full_log, lag = 12), differences = 1)

# 1.2) Entrenar el modelo manual con datos completos
# (Usamos los mismos parámetros que 'fit_arimaEst')
MODELO_FINAL_MANUAL <- arima(ipc_full_est,
                             order = c(2, 0, 2),
                             seasonal = list(order = c(1, 0, 0), period = 12),
                             include.mean = FALSE)

# 1.3) Pronosticar h=3 (Oct, Nov, Dic 2022)
fc_est_fut <- forecast(MODELO_FINAL_MANUAL, h = 3)$mean

# 1.4) Revertir las transformaciones (proceso inverso)
#   a) Revertir diff(lag=1)
y_seas_diff_full <- diff(ipc_full_log, lag = 12)
y_d1_fut <- diffinv(as.numeric(fc_est_fut), xi = tail(y_seas_diff_full, 1))[-1]

#   b) Revertir diff(lag=12)
y_lvl_log_fut <- diffinv(y_d1_fut, lag = 12, xi = tail(ipc_full_log, 12))[-(1:12)]

#   c) Revertir log (con exp)
FC_Q4_MANUAL <- exp(y_lvl_log_fut)
checkresiduals(MODELO_FINAL_MANUAL)


# --- Modelo 2: SARIMA (Auto) ---
# (Este es el modelo 'fit_sarima' que se entrenó en 'train_IPC')

# 2.1) Re-entrenar el modelo 'fit_sarima' con datos COMPLETOS
# Obtener parámetros del 'fit_sarima' original (el competidor)
ords_sarima <- arimaorder(fit_sarima)
lambda_sarima <- fit_sarima$lambda # Capturamos el lambda que auto.arima eligió

MODELO_FINAL_SARIMA <- Arima(IPC_sinO,
                             order = c(ords_sarima["p"], ords_sarima["d"], ords_sarima["q"]),
                             seasonal = c(ords_sarima["P"], ords_sarima["D"], ords_sarima["Q"]),
                             lambda = lambda_sarima, # Usamos el mismo lambda
                             biasadj = TRUE,
                             # Replicamos la configuración original de drift/constante
                             include.drift = isTRUE(fit_sarima$call$include.drift),
                             include.constant = isTRUE(fit_sarima$call$include.constant))

# 2.2) Pronosticar h=3
FC_Q4_SARIMA <- forecast(MODELO_FINAL_SARIMA, h = 3)
checkresiduals(MODELO_FINAL_SARIMA)


# --- Tabla de predicciones Q4 ---
fechas_q4 <- format(seq(as.yearmon("2022-10"), by = 1/12, length.out = 3), "%Y-%m")

pred_q4_tbl <- data.frame(
  Fecha             = fechas_q4,
  Pred_ARIMA_Manual = round(as.numeric(FC_Q4_MANUAL), 3),
  Pred_SARIMA_Auto  = round(as.numeric(FC_Q4_SARIMA$mean), 3),
  check.names       = FALSE
)

cat("\n# === PREDICCIONES FINALES Q4 2022 ===\n")
print(pred_q4_tbl, row.names = FALSE)

# FC_FUT: Pronóstico de 12 meses del SARIMA auto para el gráfico largo
FC_FUT <- forecast(MODELO_FINAL_SARIMA, h = 12) 

# FC_TEST: Pronóstico en test del SARIMA auto (de la sección de modelos)
FC_TEST <- fc_list[["SARIMA"]]

# =========================================================
# MODELO GANADOR Arima manual Mejores PRED
# =========================================================

saveRDS(pred_test_tbl, "Datos/Resultados/Pred_IPC_Test_2022.rds")
saveRDS(pred_q4_tbl,   "Datos/Resultados/Pred_IPC_2022_Q4.rds")
saveRDS(FC_TEST, "Datos/Resultados/FC_TEST_IPC.rds")
saveRDS(FC_FUT,  "Datos/Resultados/FC_FUT_IPC.rds")



# =========================================================
# ==================   G R Á F I C O S   ==================
# =========================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(zoo)
})

dir.create("Graficos", showWarnings = FALSE)

# ---------- Paleta & tema base ----------
# Paleta original para el primer gráfico
pal_original <- c(
  "Observado"       = "#2B303A",
  "Forecast SARIMA" = "#1B998B",
  "ARIMA1"          = "#E84A5F",
  "ARIMA2"          = "#F6AE2D",
  "SARIMA"          = "#1B998B",
  "ARIMAX"          = "#2D7DD2"
)

theme_set(theme_minimal(base_size = 12))
tema <- theme(
  plot.title    = element_text(face = "bold", size = 14),
  plot.subtitle = element_text(color = "grey30"),
  legend.title  = element_blank(),
  legend.position = "top",
  panel.grid.minor = element_blank()
)

# Utilidades de fechas
to_date <- function(ts_obj) as.Date(as.yearmon(time(ts_obj)))
to_date_vec <- function(x_ts) as.Date(as.yearmon(time(x_ts)))

# =========================================================
# 1) Serie completa + pronóstico futuro SARIMA (MODELO_FINAL)
# (Este código se mantiene igual que el original)
# =========================================================
df_obs <- data.frame(
  fecha = to_date(IPC_sinO),
  valor = as.numeric(IPC_sinO)
)
col80 <- colnames(FC_FUT$lower)[grepl("80", colnames(FC_FUT$lower))][1]
col95 <- colnames(FC_FUT$lower)[grepl("95", colnames(FC_FUT$lower))][1]
df_fc <- data.frame(
  fecha = to_date_vec(FC_FUT$mean),
  mean  = as.numeric(FC_FUT$mean),
  lo80  = as.numeric(FC_FUT$lower[, col80]),
  hi80  = as.numeric(FC_FUT$upper[, col80]),
  lo95  = as.numeric(FC_FUT$lower[, col95]),
  hi95  = as.numeric(FC_FUT$upper[, col95])
)
test_ini_plot1 <- to_date(test_IPC)[1]

p_full <- ggplot(df_obs, aes(x = fecha, y = valor)) +
  geom_line(aes(color = "Observado"), linewidth = 0.9) +
  annotate("rect",
           xmin = test_ini_plot1, xmax = max(df_obs$fecha),
           ymin = -Inf,   ymax = Inf,
           fill = "grey95", alpha = 0.5) +
  geom_ribbon(data = df_fc, aes(x = fecha, ymin = lo95, ymax = hi95),
              inherit.aes = FALSE,
              fill = pal_original["Forecast SARIMA"], alpha = 0.10) +
  geom_ribbon(data = df_fc, aes(x = fecha, ymin = lo80, ymax = hi80),
              inherit.aes = FALSE,
              fill = pal_original["Forecast SARIMA"], alpha = 0.20) +
  geom_line(data = df_fc, aes(x = fecha, y = mean, color = "Forecast SARIMA"),
            inherit.aes = FALSE, linewidth = 1.1) +
  scale_color_manual(values = pal_original) +
  labs(
    title    = "IPC — Serie completa y pronóstico 12 meses (SARIMA ganador)",
    subtitle = "Sombreado = tramo de test | Bandas = IC 80% / 95%",
    x = "Tiempo", y = "IPC"
  ) + tema

print(p_full)
ggsave("Graficos/01_serie_completa_mas_forecast.png", p_full, width = 10, height = 5, dpi = 150)

# ================================================
# 2) Comparativa modelos (2020+) - VERSIÓN MEJORADA
#    (incluye ARIMA manual)
# ================================================
pal_comp <- c(
  ARIMA1        = "#E15759", # Rojo
  ARIMA2        = "#F28E2B", # Naranja
  SARIMA        = "#59A14F", # Verde
  ARIMAX        = "#4E79A7", # Azul
  ARIMAEst      = "#7B2CBF", # Morado
  Observado     = "grey20"   # Gris oscuro
)

# 1) Observado completo desde 2020
obs_2020 <- window(IPC_sinO, start = c(2020,1))
df_obs_2020 <- data.frame(
  fecha = to_date(obs_2020),
  y     = as.numeric(obs_2020)
)

# 2) Predicciones SOLO en tramo de test
h_test <- length(test_IPC)
df_pred <- data.frame(
  fecha        = to_date(test_IPC),
  ARIMA1       = as.numeric(fc_arima1$mean[1:h_test]),
  ARIMA2       = as.numeric(fc_arima2$mean[1:h_test]),
  SARIMA       = as.numeric(fc_sarima$mean[1:h_test]),
  ARIMAEst     = as.numeric(fc_arimaEst_test$mean[1:h_test])
)

# Añadir ARIMAX solo si existe
if (exists("fc_MS")) {
  df_pred$ARIMAX <- as.numeric(fc_MS$mean[1:h_test])
  model_levels <- c("ARIMA1", "ARIMA2", "SARIMA", "ARIMAEst", "ARIMAX")
} else {
  model_levels <- c("ARIMA1", "ARIMA2", "SARIMA", "ARIMAEst")
}

df_pred_long <- df_pred %>%
  pivot_longer(-fecha, names_to = "Modelo", values_to = "Pred") %>%
  mutate(Modelo = factor(Modelo, levels = model_levels))

# Límites del tramo de test para sombrear
test_ini <- min(df_pred_long$fecha)
test_fin <- max(df_pred_long$fecha)

p_test_2020_mejorado <- ggplot() +
  annotate("rect",
           xmin = test_ini, xmax = test_fin,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  annotate("text",
           x = test_ini + (test_fin - test_ini) / 2,
           y = max(df_obs_2020$y, na.rm = TRUE) * 1.004, # etiqueta arriba
           label = "Tramo de Test",
           color = "grey40", fontface = "italic", size = 4) +
  geom_line(data = df_obs_2020,
            aes(x = fecha, y = y, color = "Observado"),
            linewidth = 1.3) +
  geom_line(data = df_pred_long,
            aes(x = fecha, y = Pred, color = Modelo),
            linewidth = 1.0, alpha = 0.95) +
  scale_color_manual(values = pal_comp) +
  scale_x_date(
    limits = c(as.Date("2020-01-01"), test_fin + 15),
    date_breaks = "3 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title    = "IPC: Comparativa de Modelos vs. Observado (2020-2022)",
    subtitle = "Predicciones en el tramo de test (Feb 2022 - Sep 2022)",
    x = "Fecha (año-mes)", y = "IPC (Índice)", color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 11),
    axis.title         = element_text(size = 12),
    legend.position    = "top",
    legend.text        = element_text(size = 12),
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey30", size = 13, margin = margin(b = 10)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey85")
  )

print(p_test_2020_mejorado)
ggsave("Graficos/02_comparativa_test_modelos_2020_MEJORADO.png",
       p_test_2020_mejorado, width = 12, height = 6, dpi = 150)
