### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL IPC

# Cargar librerias
library(zoo)
library(cowplot)
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


# SARIMA "forzado" a considerar componente estacional si la hay
fit_sarima <- auto.arima(train_IPC, seasonal = TRUE,
                         lambda = lambda, stepwise = FALSE, approximation = FALSE)

checkresiduals(fit_arima1)
checkresiduals(fit_arimaEst)
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
fc_sarima <- forecast(fit_sarima, h = h)

fc_list <- list(
  `ARIMA1` = forecast(fit_arima1, h=h),
  `ARIMAEst` = structure(list(mean = fc_arimaEst_lvl), class = "forecast"),
  `SARIMA` = forecast(fit_sarima, h=h)
)

# Comparación rápida contra test
autoplot(fc_arima1) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA1 vs Test")
autoplot(fc_arimaEst_lvl) + autolayer(test_IPC, series="Test") + ggtitle("ARIMAEst vs Test")
autoplot(fc_sarima) + autolayer(test_IPC, series="Test") + ggtitle("SARIMA vs Test")

accuracy(fc_arima1$mean, test_IPC)
accuracy(fc_arimaEst_lvl, test_IPC)
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
fit_map <- list(`ARIMA1`=fit_arima1, `SARIMA`=fit_sarima)
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
ggtsdisplay(residuals(WIN_FIT))
checkresiduals(WIN_FIT)


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
  SARIMA    = round(as.numeric(fc_sarima_test$mean)[seq_len(n)], 3),
  ARIMAX    = round(as.numeric(fc_MS$mean)[seq_len(n)], 3),
  check.names = FALSE
)
print(pred_test_tbl, row.names = FALSE)
# ===== Tabla comparativa en TEST (ME, RMSE, MAE, MPE, MAPE, ACF1, AIC) =====
get_row <- function(nombre, fc_vals, model_obj, y_test){
  err  <- as.numeric(y_test) - as.numeric(fc_vals)
  data.frame(
    Modelo = nombre,
    ME   = mean(err, na.rm=TRUE),
    RMSE = sqrt(mean(err^2, na.rm=TRUE)),
    MAE  = mean(abs(err), na.rm=TRUE),
    MPE  = mean(100 * err / as.numeric(y_test), na.rm=TRUE),
    MAPE = mean(100 * abs(err / as.numeric(y_test)), na.rm=TRUE),
    ACF1 = as.numeric(acf(err, lag.max=1, plot=FALSE)$acf[2]),
    AIC  = suppressWarnings(AIC(model_obj)),
    check.names = FALSE
  )
}

h_test <- length(test_IPC)
tabla_test <- do.call(rbind, list(
  get_row("ARIMA1",            fc_arima1$mean[1:h_test],  fit_arima1,   test_IPC),
  get_row("SARIMA",            fc_sarima$mean[1:h_test],  fit_sarima,   test_IPC),
  get_row("ARIMA manual",      fc_arimaEst_lvl[1:h_test], fit_arimaEst, test_IPC),
  get_row("ARIMAX MS",         fc_MS$mean[1:h_test],      ARIMAX_MS,    test_IPC),
  get_row("ARIMAX MS+SMI",     fc_MSS$mean[1:h_test],     ARIMAX_MSS,   test_IPC),
  get_row("ARIMAX MS+SMI+UR",  fc_MSSU$mean[1:h_test],    ARIMAX_MSSU,  test_IPC)
))

tabla_test <- tabla_test[order(tabla_test$RMSE), ]
cat("\n# === Comparativa de modelos en TEST (ordenados por RMSE) ===\n")
print(tabla_test, row.names = FALSE)
saveRDS(tabla_test,"Datos/Resultados/Tabla_Antes_CV_IPC.rds")
################################################################################
# 7. CROSS-VALIDATION (Rolling forecast) con los modelos de IPC (h=1)
################################################################################
# --- Parámetros de CV ---
# h <- 1
# n <- length(IPC_sinO)
# train_size <- max(84, 2*frequency(IPC_sinO) + 24)
# 
# lambda_cv <- BoxCox.lambda(window(IPC_sinO, end = time(IPC_sinO)[train_size]))
# 
# m <- n - train_size
# pred_arima1_cv       <- rep(NA_real_, m)
# pred_sarima_cv       <- rep(NA_real_, m)
# pred_arimaEst_cv     <- rep(NA_real_, m)
# pred_arimax_MS_cv    <- rep(NA_real_, m)
# pred_arimax_MSS_cv   <- rep(NA_real_, m)
# pred_arimax_MSSU_cv  <- rep(NA_real_, m)
# 
# fix_na <- function(v) if (anyNA(v)) forecast::na.interp(v) else v
# 
# pb <- txtProgressBar(min = 0, max = m, style = 3, width = 50, char = "=")
# k <- 0
# for (i in train_size:(n-1)) {
#   idx_pred <- i - train_size + 1
#   
#   # ----- Ventanas target y exógenas -----
#   y_tr   <- window(IPC_sinO, end = time(IPC_sinO)[i])
#   ms_tr  <- window(MS_sinO,  end = time(MS_sinO)[i])
#   smi_tr <- window(SMI_sinO, end = time(SMI_sinO)[i])
#   ur_tr  <- window(UR_sinO,  end = time(UR_sinO)[i])
#   ms_nx  <- if (i+1 <= length(MS_sinO))  MS_sinO[i+1]  else NA_real_
#   smi_nx <- if (i+1 <= length(SMI_sinO)) SMI_sinO[i+1] else NA_real_
#   ur_nx  <- if (i+1 <= length(UR_sinO))  UR_sinO[i+1]  else NA_real_
#   
#   # Limpieza básica
#   y_tr   <- fix_na(y_tr)
#   ms_tr  <- fix_na(ms_tr);  smi_tr <- fix_na(smi_tr);  ur_tr <- fix_na(ur_tr)
#   
#   # ---------- ARIMA1 ----------
#   fit_a1 <- try(auto.arima(y_tr, seasonal = FALSE, lambda = lambda_cv,
#                            stepwise = TRUE, approximation = TRUE), silent = TRUE)
#   if (!inherits(fit_a1, "try-error"))
#     pred_arima1_cv[idx_pred] <- as.numeric(forecast(fit_a1, h = h)$mean)
#   
#   # ---------- SARIMA ----------
#   fit_s  <- try(auto.arima(y_tr, seasonal = TRUE, lambda = lambda_cv,
#                            stepwise = TRUE, approximation = TRUE), silent = TRUE)
#   if (!inherits(fit_s, "try-error"))
#     pred_sarima_cv[idx_pred] <- as.numeric(forecast(fit_s, h = h)$mean)
#   
#   # ---------- ARIMA manual (log, ∇, ∇12) ----------
#   xlog <- log(y_tr)
#   ydf  <- diff(diff(xlog, lag = 12), differences = 1)
#   fit_m <- try(Arima(ydf, order = c(2,0,2),
#                      seasonal = list(order = c(1,0,0), period = 12),
#                      include.mean = FALSE), silent = TRUE)
#   if (!inherits(fit_m, "try-error")) {
#     fc_m <- forecast(fit_m, h = h)$mean
#     y_seas <- diff(xlog, lag = 12)
#     y_d1   <- diffinv(as.numeric(fc_m), xi = tail(y_seas, 1))[-1]
#     y_lvl  <- diffinv(y_d1, lag = 12, xi = tail(xlog, 12))[-(1:12)]
#     pred_arimaEst_cv[idx_pred] <- exp(y_lvl)[1]
#   }
#   
#   # ---------- ARIMAX (MS) ----------
#   mu_MS  <- mean(as.numeric(ms_tr), na.rm = TRUE)
#   sd_MS  <- sd(as.numeric(ms_tr), na.rm = TRUE)
#   X_MS   <- scale(matrix(as.numeric(ms_tr), ncol = 1), center = mu_MS, scale = sd_MS)
#   Xn_MS  <- scale(matrix(ms_nx, nrow = 1), center = mu_MS, scale = sd_MS)
#   
#   fit_x_MS <- try(auto.arima(y_tr, xreg = X_MS, lambda = lambda_cv,
#                              seasonal = TRUE, stepwise = TRUE, approximation = TRUE),
#                   silent = TRUE)
#   if (!inherits(fit_x_MS, "try-error") && is.finite(Xn_MS)) {
#     pred_arimax_MS_cv[idx_pred] <- as.numeric(forecast(fit_x_MS, xreg = Xn_MS, h = h, biasadj = TRUE)$mean)
#   }
#   
#   # ---------- ARIMAX (MS+SMI) ----------
#   Xtr_MSS <- cbind(MS = as.numeric(ms_tr), SMI = as.numeric(smi_tr))
#   mu_MSS  <- colMeans(Xtr_MSS, na.rm = TRUE)
#   sd_MSS  <- apply(Xtr_MSS, 2, sd, na.rm = TRUE)
#   X_MSS   <- scale(Xtr_MSS, center = mu_MSS, scale = sd_MSS)
#   Xn_MSS  <- scale(matrix(c(ms_nx, smi_nx), nrow = 1), center = mu_MSS, scale = sd_MSS)
#   
#   fit_x_MSS <- try(auto.arima(y_tr, xreg = X_MSS, lambda = lambda_cv,
#                               seasonal = TRUE, stepwise = TRUE, approximation = TRUE),
#                    silent = TRUE)
#   if (!inherits(fit_x_MSS, "try-error") && all(is.finite(Xn_MSS))) {
#     pred_arimax_MSS_cv[idx_pred] <- as.numeric(forecast(fit_x_MSS, xreg = Xn_MSS, h = h, biasadj = TRUE)$mean)
#   }
#   
#   # ---------- ARIMAX (MS+SMI+UR) ----------
#   Xtr_MSSU <- cbind(MS = as.numeric(ms_tr), SMI = as.numeric(smi_tr), UR = as.numeric(ur_tr))
#   mu_MSSU  <- colMeans(Xtr_MSSU, na.rm = TRUE)
#   sd_MSSU  <- apply(Xtr_MSSU, 2, sd, na.rm = TRUE)
#   X_MSSU   <- scale(Xtr_MSSU, center = mu_MSSU, scale = sd_MSSU)
#   Xn_MSSU  <- scale(matrix(c(ms_nx, smi_nx, ur_nx), nrow = 1), center = mu_MSSU, scale = sd_MSSU)
#   
#   fit_x_MSSU <- try(auto.arima(y_tr, xreg = X_MSSU, lambda = lambda_cv,
#                                seasonal = TRUE, stepwise = TRUE, approximation = TRUE),
#                     silent = TRUE)
#   if (!inherits(fit_x_MSSU, "try-error") && all(is.finite(Xn_MSSU))) {
#     pred_arimax_MSSU_cv[idx_pred] <- as.numeric(forecast(fit_x_MSSU, xreg = Xn_MSSU, h = h, biasadj = TRUE)$mean)
#   }
#   
#   k <- k + 1; setTxtProgressBar(pb, k)
# }
# close(pb)
# 
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
# 
# ##############################################################################
# # 8. Visualización de Cross-Validation (IPC mensual)
# ##############################################################################
# 
# # --- Utilidades ---
# align_vec <- function(v, len){
#   # Devuelve los 'len' últimos valores de v como numérico
#   if (is.null(v)) return(NULL)
#   v <- as.numeric(v)
#   if (length(v) < len) return(NULL)
#   tail(v, len)
# }
# # Índices/fechas del tramo de CV
# n <- length(IPC_sinO)
# start_index_cv <- train_size + 1
# Fechas_cv <- time(IPC_sinO)[start_index_cv:n]
# Real_cv   <- as.numeric(window(IPC_sinO, start = time(IPC_sinO)[start_index_cv]))
# L <- length(Real_cv)
# 
# # # Alinear predicciones a la longitud del tramo CV (por si hay NA iniciales)
# preds <- list(
#   `ARIMA (auto)`          = align_vec(pred_arima1_cv,      L),
#   `SARIMA (auto)`         = align_vec(pred_sarima_cv,      L),
#   `ARIMA manual (log)`    = align_vec(pred_arimaEst_cv,    L),
#   `ARIMAX MS`             = align_vec(pred_arimax_MS_cv,   L),
#   `ARIMAX MS+SMI`         = align_vec(pred_arimax_MSS_cv,  L),
#   `ARIMAX MS+SMI+UR`      = align_vec(pred_arimax_MSSU_cv, L)
# )
# 
# # Construir data.frames solo para los que existen
# df_cv_list <- lapply(names(preds), function(nm){
#   pv <- preds[[nm]]
#   if (is.null(pv)) return(NULL)
#   data.frame(Fecha = Fechas_cv, Real = Real_cv, Pred = pv, Modelo = nm)
# })
# df_cv_list <- Filter(Negate(is.null), df_cv_list)
# 
# # Si no hay ningún modelo disponible, salimos con aviso
# if (length(df_cv_list) == 0) {
#   message("No se encontraron vectores de predicción de CV (pred_*_cv).")
# } else {
#   
#   # Función de trazado: Real (negro) vs Pred (color) con línea discontinua
#   plot_cv <- function(df, title, color) {
#     ggplot(df, aes(x = as.Date(as.yearmon(Fecha)))) +
#       geom_line(aes(y = Real), color = "black", linewidth = 0.9) +
#       geom_line(aes(y = Pred), color = color, linetype = "dashed", linewidth = 0.9) +
#       labs(
#         title = paste0(title, " — Cross-Validation"),
#         y = "IPC", x = "Mes"
#       ) +
#       scale_x_date( limits = as.Date(c("2000-01-01", "2022-12-31")),date_breaks = "2 years", date_labels = "%Y") +
#       theme_minimal(base_size = 12) +
#       theme(panel.grid.minor = element_blank())
#   }
#   
#   
#   # Paleta fija para mantener consistencia
#   col_map <- c(
#     `ARIMA (auto)`       = "#E15759",
#     `SARIMA (auto)`      = "#59A14F",
#     `ARIMA manual (log)` = "#7B2CBF",
#     `ARIMAX MS`          = "#4E79A7",
#     `ARIMAX MS+SMI`      = "#F28E2B",
#     `ARIMAX MS+SMI+UR`   = "#76B7B2"
#   )
#   
#   # Crear una lista de plots en orden
#   plots <- lapply(df_cv_list, function(df){
#     nm <- unique(df$Modelo)
#     plot_cv(df, nm, col_map[[nm]])
#   })
#   
#   # Mostrar en cuadrícula 2x3 (o lo que toque según cuántos haya)
#   # Si hay 1 solo, lo imprime sin grid.arrange
#   if (length(plots) == 1) {
#     print(plots[[1]])
#   } else {
#     do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
#   }
# }
# final_plot_cv <- if (length(plots) == 1) {
#   plots[[1]]
# } else {
#   gridExtra::arrangeGrob(grobs = plots, ncol = 2)
# }
# 
# ggsave("Graficos/CrossValidation_IPC.png", final_plot_cv, width = 12, height = 8, dpi = 150)

tab_cv <- readRDS("Datos/Resultados/CV_metrics_IPC.rds")
print(tab_cv, row.names = FALSE)
# 
# # Observado a partir del primer punto pronosticado
# actual_cv <- window(IPC_sinO, start = time(IPC_sinO)[train_size + 1])
# len <- length(actual_cv)
# 
# # Alinear longitudes
# pred_arima1_cv      <- tail(pred_arima1_cv,      len)
# pred_sarima_cv      <- tail(pred_sarima_cv,      len)
# pred_arimaEst_cv    <- tail(pred_arimaEst_cv,    len)
# pred_arimax_MS_cv   <- tail(pred_arimax_MS_cv,   len)
# pred_arimax_MSS_cv  <- tail(pred_arimax_MSS_cv,  len)
# pred_arimax_MSSU_cv <- tail(pred_arimax_MSSU_cv, len)
# 
# # Errores
# e1   <- actual_cv - pred_arima1_cv
# e2   <- actual_cv - pred_sarima_cv
# e3   <- actual_cv - pred_arimaEst_cv
# e_MS <- actual_cv - pred_arimax_MS_cv
# e_MSS<- actual_cv - pred_arimax_MSS_cv
# e4   <- actual_cv - pred_arimax_MSSU_cv  # (mantengo tu nombre original)
# 
# # Funciones auxiliares
# cv_row <- function(nombre, err, actual, aic_val){
#   idx <- which(is.finite(err) & is.finite(actual))
#   e <- err[idx]; a <- as.numeric(actual)[idx]
#   acf1 <- if (length(na.omit(e)) >= 2)
#     as.numeric(acf(na.omit(e), lag.max = 1, plot = FALSE)$acf[2]) else NA_real_
#   data.frame(
#     Modelo = nombre,
#     ME   = mean(e, na.rm = TRUE),
#     RMSE = sqrt(mean(e^2, na.rm = TRUE)),
#     MAE  = mean(abs(e), na.rm = TRUE),
#     MPE  = mean(100 * e / a, na.rm = TRUE),
#     MAPE = mean(100 * abs(e / a), na.rm = TRUE),
#     ACF1 = acf1,
#     AIC  = aic_val,
#     check.names = FALSE
#   )
# }
# safe_AIC <- function(m) tryCatch(AIC(m), error = function(...) NA_real_)
# 
# AIC_map <- c(
#   "ARIMA1"            = safe_AIC(fit_arima1),
#   "SARIMA"            = safe_AIC(fit_sarima),
#   "ARIMA manual"      = safe_AIC(fit_arimaEst),
#   "ARIMAX MS"         = safe_AIC(ARIMAX_MS),
#   "ARIMAX MS+SMI"     = safe_AIC(ARIMAX_MSS),
#   "ARIMAX MS+SMI+UR"  = safe_AIC(ARIMAX_MSSU)
# )
# 
# tabla_cv <- do.call(rbind, list(
#   cv_row("ARIMAX MS",         e_MS,     actual_cv, AIC_map["ARIMAX MS"]),
#   cv_row("ARIMAX MS+SMI",     e_MSS,    actual_cv, AIC_map["ARIMAX MS+SMI"]),
#   cv_row("ARIMAX MS+SMI+UR",  e4,       actual_cv, AIC_map["ARIMAX MS+SMI+UR"]),
#   cv_row("SARIMA",            e2,       actual_cv, AIC_map["SARIMA"]),
#   cv_row("ARIMA manual",      e3,       actual_cv, AIC_map["ARIMA manual"]),
#   cv_row("ARIMA1",            e1,       actual_cv, AIC_map["ARIMA1"])
# ))
# tabla_cv <- tabla_cv[order(tabla_cv$RMSE), ]
# print(tabla_cv, row.names = FALSE)
# saveRDS(tabla_cv,"Datos/Resultados/Tabla_Despues_CV_IPC.rds")
tabla_cv <- readRDS("Datos/Resultados/Tabla_Despues_CV_IPC.rds")
print(tabla_cv, row.names = FALSE)

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
# =========================================================

fc_manual_q4 <- forecast(MODELO_FINAL_MANUAL, h = 3, level = c(80, 95))

y_seas_diff_full <- diff(ipc_full_log, lag = 12)
inv_to_level <- function(x_vec) {
  y_d1  <- diffinv(as.numeric(x_vec), xi = tail(y_seas_diff_full, 1))[-1]
  y_lvl <- diffinv(y_d1, lag = 12, xi = tail(ipc_full_log, 12))[-(1:12)]
  exp(y_lvl)
}

# Invertir media e intervalos
mean_lvl <- inv_to_level(fc_manual_q4$mean)
lo80     <- inv_to_level(fc_manual_q4$lower[,"80%"])
hi80     <- inv_to_level(fc_manual_q4$upper[,"80%"])
lo95     <- inv_to_level(fc_manual_q4$lower[,"95%"])
hi95     <- inv_to_level(fc_manual_q4$upper[,"95%"])

# Crear objeto tipo 'forecast' en la escala original
FC_Q4_MANUAL_FC <- structure(list(
  mean  = ts(mean_lvl, start = c(2022, 10), frequency = frequency(IPC_sinO)),
  lower = cbind("80%" = lo80, "95%" = lo95),
  upper = cbind("80%" = hi80, "95%" = hi95),
  level = c(80, 95),
  x     = IPC_sinO
), class = "forecast")

df_total <- data.frame(
  Trimestre = as.Date(as.yearmon(time(IPC_sinO))),
  PIB       = as.numeric(IPC_sinO)
)

df_pred2 <- data.frame(
  Trimestre = as.Date(as.yearmon(time(FC_Q4_MANUAL_FC$mean))),
  Pred      = as.numeric(FC_Q4_MANUAL_FC$mean),
  Lower80   = as.numeric(FC_Q4_MANUAL_FC$lower[,"80%"]),
  Upper80   = as.numeric(FC_Q4_MANUAL_FC$upper[,"80%"]),
  Lower95   = as.numeric(FC_Q4_MANUAL_FC$lower[,"95%"]),
  Upper95   = as.numeric(FC_Q4_MANUAL_FC$upper[,"95%"])
)

# Unión visual (último observado → primer pronóstico)
df_union <- data.frame(
  Trimestre = c(tail(df_total$Trimestre,1), df_pred2$Trimestre[1]),
  Pred      = c(tail(df_total$PIB,1),       df_pred2$Pred[1])
)

# Rango para el zoom
x_min <- as.Date("2022-06-01")
x_max <- as.Date("2022-12-31")

df_total_zoom <- subset(df_total, Trimestre >= x_min & Trimestre <= x_max)
df_pred2_zoom <- subset(df_pred2, Trimestre >= x_min & Trimestre <= x_max)

breaks_all  <- seq(x_min, x_max, by = "1 month")
labels_all  <- format(breaks_all, "%Y-%m")


p_main <- ggplot() +
  # Serie observada
  geom_line(data = df_total, aes(x = Trimestre, y = PIB),
            color = "#4E2869", linewidth = 1) +
  # Segmento de unión
  geom_line(data = df_union, aes(Trimestre, Pred),
            color = "#8BC53F", linewidth = 1, linetype = "dashed") +
  # Bandas de confianza 95 % y 80 %
  geom_ribbon(data = df_pred2, aes(x = Trimestre, ymin = Lower95, ymax = Upper95),
              fill = "#8BC53F", alpha = 0.1) +
  geom_ribbon(data = df_pred2, aes(x = Trimestre, ymin = Lower80, ymax = Upper80),
              fill = "#8BC53F", alpha = 0.2) +
  # Línea de predicción
  geom_line(data = df_pred2, aes(x = Trimestre, y = Pred),
            color = "#8BC53F", linewidth = 1, linetype = "dashed") +
  scale_x_date(limits = as.Date(c("2000-01-01","2023-12-31")),
               date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Predicción IPC Q4 2022 (ARIMA manual)",
    y = "IPC_sinO", x = "Fecha"
  ) +
  theme_minimal()

p_zoom <- ggplot() +
  # Serie observada
  geom_line(data = df_total_zoom, aes(x = Trimestre, y = PIB),
            color = "#4E2869", linewidth = 1) +
  # Segmento de unión
  geom_line(data = subset(df_union, Trimestre >= x_min & Trimestre <= x_max),
            aes(Trimestre, Pred),
            color = "#8BC53F", linewidth = 1, linetype = "dashed") +
  # Bandas 95 % y 80 %
  geom_ribbon(data = df_pred2_zoom, aes(x = Trimestre, ymin = Lower95, ymax = Upper95),
              fill = "#8BC53F", alpha = 0.1) +
  geom_ribbon(data = df_pred2_zoom, aes(x = Trimestre, ymin = Lower80, ymax = Upper80),
              fill = "#8BC53F", alpha = 0.2) +
  # Línea de predicción
  geom_line(data = df_pred2_zoom, aes(x = Trimestre, y = Pred),
            color = "#8BC53F", linewidth = 1, linetype = "dashed") +
  coord_cartesian(xlim = c(x_min, x_max), expand = FALSE) +
  scale_x_date(breaks = breaks_all, labels = labels_all, limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(color = "#C01757", linewidth = 0.5)
  )

final_plot <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_zoom, x = 0.60, y = 0.15, width = 0.35, height = 0.35)

final_plot

ggsave("Graficos/Pred_IPC_Q4.png", final_plot, width = 12, height = 6, dpi = 150)

