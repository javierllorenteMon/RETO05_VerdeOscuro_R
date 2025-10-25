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
                      order    = c(0, 0, 3),     # d=0 porque ya está diferenciada
                      seasonal = c(0, 0, 2),     # D=0 porque ya está diferenciada estacionalmente
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

fc_arima1 <- forecast(fit_arima1, h = h)
fc_arimaEst <- forecast(fit_arimaEst, h = h)
fc_arima2 <- forecast(fit_arima2, h = h)
fc_sarima <- forecast(fit_sarima, h = h)

fc_list <- list(
  `ARIMA1` = forecast(fit_arima1, h=h),
  `ARIMAEst` = forecast(fc_arimaEst, h=h),
  `ARIMA2` = forecast(fit_arima2, h=h),
  `SARIMA` = forecast(fit_sarima, h=h)
)

# Comparación rápida contra test
autoplot(fc_arima1) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA1 vs Test")
autoplot(fc_arimaEst) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA2 vs Test")
autoplot(fc_arima2) + autolayer(test_IPC, series="Test") + ggtitle("ARIMA2 vs Test")
autoplot(fc_sarima) + autolayer(test_IPC, series="Test") + ggtitle("SARIMA vs Test")

accuracy(fc_arima1$mean, test_IPC)
accuracy(fc_arimaEst$mean, test_IPC)
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

stopifnot(length(train_MS)  == length(train_IPC),
          length(train_SMI) == length(train_IPC),
          length(train_UR)  == length(train_IPC),
          length(test_MS)   == length(test_IPC),
          length(test_SMI)  == length(test_IPC),
          length(test_UR)   == length(test_IPC))

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


# Comparativa ARIMAX VS SARIMA pronostico

# 1) Reentrena ARIMAX_MS con TODA la serie IPC usando MS estandarizada
MS_full    <- fix_na(MS_sinO)
X_full_raw <- as.matrix(as.numeric(MS_full))
X_full     <- std_apply(X_full_raw, S_MS$mu, S_MS$sdv)

ARIMAX_MS_FULL <- auto.arima(IPC_sinO,
                             xreg = X_full,
                             lambda = lambda, biasadj = TRUE,
                             seasonal = TRUE, 
                             allowdrift = TRUE,
                             stepwise = FALSE, approximation = FALSE)

# 2) Pronostica MS para oct–nov–dic 2022 (h=3) y úsalo como xreg futuro
fit_MS      <- auto.arima(MS_full, stepwise = FALSE, approximation = FALSE)
MS_q4_hat   <- forecast(fit_MS, h = 3)$mean

X_q4_raw <- as.matrix(as.numeric(MS_q4_hat))
X_q4     <- std_apply(X_q4_raw, S_MS$mu, S_MS$sdv)

# 3) Forecast ARIMAX_MS para Q4 (3 pasos)
FC_Q4 <- forecast(ARIMAX_MS_FULL, xreg = X_q4, h = 3)


#Predicciones a FUTURO SARIMA
ords <- arimaorder(WIN_FIT)
nc_drift <- isTRUE(WIN_FIT$call$include.drift)
inc_const <- isTRUE(WIN_FIT$call$include.constant)

MODELO_FINAL <- Arima(IPC_sinO,
                      order    = c(ords["p"], ords["d"], ords["q"]),
                      seasonal = c(ords["P"], ords["D"], ords["Q"]),
                      lambda   = lambda, biasadj = TRUE,
                      include.drift    = inc_drift,
                      include.constant = inc_const)

FC_TEST <- fc_list[[winner_name]]
FC_FUT  <- forecast(MODELO_FINAL, h=12)
checkresiduals(MODELO_FINAL)

#TABLA COMPARACIONES TODOS LOS MODELOS TEST
fc_arima_test  <- fc_list[["ARIMA1"]]
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
  ARIMA2    = round(as.numeric(fc_arima2_test$mean)[seq_len(n)], 3),
  SARIMA    = round(as.numeric(fc_sarima_test$mean)[seq_len(n)], 3),
  ARIMAX    = round(as.numeric(fc_MS$mean)[seq_len(n)], 3),
  check.names = FALSE
)
print(pred_test_tbl, row.names = FALSE)

# Crear vector de fechas para los meses de octubre, noviembre y diciembre de 2022
fechas_q4 <- format(seq(as.yearmon("2022-10"), by = 1/12, length.out = 3), "%Y-%m")

#PREDICCIONES DE MESES 10,11,12
pred_q4_tbl <- data.frame(
  Fecha          = fechas_q4,
  Pred_ARIMAX_MS = round(as.numeric(FC_Q4$mean), 3),
  Pred_SARIMA    = round(as.numeric(FC_FUT$mean[1:3]), 3),
  check.names    = FALSE
)

print(pred_q4_tbl, row.names = FALSE)
checkresiduals(ARIMAX_MS_FULL)

# =========================================================
# MODELO GANADOR ARIMAX PERO NOS QUEDAMOS CON EL MAS SIMPLE(SARIMA)
# =========================================================

saveRDS(pred_test_tbl, "Datos/Resultados/Pred_IPC_Test_2022.rds")
saveRDS(pred_q4_tbl,   "Datos/Resultados/Pred_IPC_2022_Q4.rds")


