### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL PIB

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

`%||%` <- function(a,b) if (is.null(a)) b else a

# ------ Configuracion ------

RUTA_RDS   <- "Datos/transformados/IPC_sinO_M.rds"
TRAIN_START <- c(2000, 1)
TRAIN_END   <- c(2022, 1)   # incluye parte del nuevo régimen
TEST_START  <- c(2022, 2)
TEST_END    <- c(2022, 9)
FUTURE_H    <- 12

# ------ Datos ------

IPC_sinO <- readRDS(RUTA_RDS)
stopifnot(is.ts(IPC_sinO), frequency(IPC_sinO) == 12)

train_IPC <- window(IPC_sinO, start = TRAIN_START, end = TRAIN_END)
test_IPC  <- window(IPC_sinO, start = TEST_START,  end = TEST_END)
h_test    <- length(test_IPC)
if (h_test <= 0) stop("Tramo TEST vacío. Revisa TEST_START/TEST_END.")

# ------ Identificacion ------

lambda <- BoxCox.lambda(train_IPC)
x_bc   <- BoxCox(train_IPC, lambda)
d      <- ndiffs(x_bc)
D      <- nsdiffs(x_bc)
cat(sprintf("Lambda=%.3f | d=%d | D=%d | m=12\n", lambda, d, D))

safe_lb <- function(model, lag = 24, fitdf = NULL) {
  res <- try({
    if (is.null(fitdf)) fitdf <- length(coef(model))
    Box.test(residuals(model), lag = lag, type = "Ljung-Box", fitdf = fitdf)$p.value
  }, silent = TRUE)
  if (inherits(res, "try-error")) NA_real_ else as.numeric(res)
}

# ------ Modelos ------

fit_arima1 <- auto.arima(train_IPC, lambda=lambda, biasadj=TRUE,
                         seasonal=FALSE, d=d, stepwise=FALSE, approximation=FALSE)

fit_arima2 <- auto.arima(train_IPC, lambda=lambda, biasadj=TRUE,
                         seasonal=FALSE, d=d, max.p=6, max.q=6, allowdrift=FALSE,
                         stepwise=FALSE, approximation=FALSE)

fit_sarima <- auto.arima(train_IPC, lambda=lambda, biasadj=TRUE,
                         seasonal=TRUE,  d=d, D=D, stepwise=FALSE, approximation=FALSE)

# ------ Pronostico Test ------

fc_list <- list(
  `ARIMA1` = forecast(fit_arima1, h=h_test),
  `ARIMA2` = forecast(fit_arima2, h=h_test),
  `SARIMA` = forecast(fit_sarima,  h=h_test)
)

# ------ Metricas + LjungBox ------

lb_map <- c(
  `ARIMA1` = safe_lb(fit_arima1, lag=24),
  `ARIMA2` = safe_lb(fit_arima2, lag=24),
  `SARIMA` = safe_lb(fit_sarima,  lag=24)
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
cat("\nGanador provisional:", winner_name,
    ifelse((acc_tab %>% filter(Modelo==winner_name))$LjungBox_p<0.05,
           " (no pasa Ljung-Box; elegido por RMSE)", ""), "\n")

fit_map <- list(`ARIMA1`=fit_arima1, `ARIMA2`=fit_arima2, `SARIMA`=fit_sarima)
WIN <- fit_map[[winner_name]]

# ------ Reentrenar a toda la serie y pronostico a futuro ------

MODELO_FINAL <- Arima(IPC_sinO,
                      order   = WIN$arma[c(1,6,2)],  # p,d,q
                      seasonal= list(order=WIN$arma[c(3,7,4)], period=frequency(IPC_sinO)),
                      include.drift   = WIN$call$include.drift %||% FALSE,
                      include.constant= WIN$call$include.constant %||% FALSE,
                      lambda=lambda, biasadj=TRUE)

FC_TEST <- fc_list[[winner_name]]
FC_FUT  <- forecast(MODELO_FINAL, h=FUTURE_H)


# ------  Métricas claras + tabla de valores TEST ------

ACC_FINAL <- accuracy(FC_TEST, test_IPC)[c("Training set","Test set"),
                                         c("ME","RMSE","MAE","MAPE")]
cat("\n# === Accuracy (TRAIN/TEST) del ganador ===\n")
print(ACC_FINAL)

fechas_test <- as.yearmon(time(test_IPC))
pred_test_tbl <- data.frame(
  Fecha_YM      = format(fechas_test, "%Y-%m"),
  Observado     = round(as.numeric(test_IPC), 3),
  `Pred ganador`= round(as.numeric(FC_TEST$mean), 3),
  check.names   = FALSE
)
cat("\n# === Tabla de predicciones en TEST (ganador) ===\n")
print(pred_test_tbl, row.names=FALSE)

cat("\nGanador definitivo:", winner_name,
    "| RMSE test =", round((acc_tab %>% filter(Modelo==winner_name))$RMSE,3),
    "| Ljung-Box p =", round((acc_tab %>% filter(Modelo==winner_name))$LjungBox_p,4), "\n")
