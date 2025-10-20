### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DEL IPC

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
h_test    <- length(test_IPC) + 2   # <- Ajuste pedido (+2)
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
checkresiduals(MODELO_FINAL)

# ------  Métricas claras + tabla de valores TEST ------

ACC_FINAL <- accuracy(FC_TEST, test_IPC)[c("Training set","Test set"),
                                         c("ME","RMSE","MAE","MAPE")]
cat("\n# === Accuracy (TRAIN/TEST) del ganador ===\n")
print(ACC_FINAL)

fechas_test <- as.yearmon(time(test_IPC))
pred_test_tbl <- data.frame(
  Fecha_YM      = format(fechas_test, "%Y-%m"),
  Observado     = round(as.numeric(test_IPC), 3),
  `Pred ganador`= round(as.numeric(FC_TEST$mean[1:length(test_IPC)]), 3),
  check.names   = FALSE
)
cat("\n# === Tabla de predicciones en TEST (ganador) ===\n")
print(pred_test_tbl, row.names=FALSE)

cat("\nGanador definitivo:", winner_name,
    "| RMSE test =", round((acc_tab %>% filter(Modelo==winner_name))$RMSE,3),
    "| Ljung-Box p =", round((acc_tab %>% filter(Modelo==winner_name))$LjungBox_p,4), "\n")

# =========================================================
# DIAGNÓSTICO DE RESIDUOS (GANADOR PROVISIONAL y MODELO FINAL)
# =========================================================
cat("\n# === Diagnóstico residuos — GANADOR PROVISIONAL (ajustado en TRAIN) ===\n")
ggtsdisplay(residuals(WIN))
checkresiduals(WIN)

cat("\n# === Diagnóstico residuos — MODELO FINAL (reentrenado en TODA la serie) ===\n")
ggtsdisplay(residuals(MODELO_FINAL))
checkresiduals(MODELO_FINAL)

# =========================================================
# GRÁFICOS DE PRONÓSTICO (TEST y FUTURO)
# =========================================================
library(ggplot2)
library(forecast)
dir.create("Graficos", showWarnings = FALSE)

h_test <- length(test_IPC) + 2
fc_arima_test   <- forecast(fit_arima1, h = h_test)
fc_arima2_test  <- forecast(fit_arima2, h = h_test)
fc_sarima_test  <- forecast(fit_sarima, h = h_test)

p_test_full <- autoplot(window(IPC_sinO, start = c(2000,1)), series = "IPC") +
  autolayer(fc_sarima_test$mean, series = "SARIMA (test)") +
  autolayer(fc_arima_test$mean,  series = "ARIMA1 (test)") +
  autolayer(fc_arima2_test$mean, series = "ARIMA2 (test)") +
  autolayer(test_IPC,            series = "Observado test") +
  ggtitle("IPC — Comparativa pronósticos en tramo de test") +
  xlab("Tiempo") + ylab("IPC") +
  theme(legend.title = element_blank())
print(p_test_full)

p_test_zoom <- p_test_full + coord_cartesian(xlim = c(2017, 2023.99)) +
  ggtitle("IPC — Comparativa pronósticos en test (zoom 2017–2023)")
print(p_test_zoom)

p_future <- autoplot(FC_FUT) +
  coord_cartesian(xlim = c(2000, max(time(FC_FUT$mean)))) +
  ggtitle("IPC — Pronóstico 12 meses (modelo final)") +
  xlab("Tiempo") + ylab("IPC")
print(p_future)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  library(gridExtra)
  p_combo <- grid.arrange(p_test_zoom, p_future, ncol = 1,
                          top = "IPC — Pronósticos: Test (zoom) y Futuro 12 meses")
}

df_comp <- data.frame(
  Fecha  = as.numeric(time(test_IPC)),
  Real   = as.numeric(test_IPC),
  ARIMA1 = as.numeric(fc_arima_test$mean[1:length(test_IPC)]),
  SARIMA = as.numeric(fc_sarima_test$mean[1:length(test_IPC)])
)

p1 <- ggplot(df_comp, aes(x = Fecha)) +
  geom_line(aes(y = Real),  color = "blue", size = 1.2) +
  geom_line(aes(y = ARIMA1), color = "red",  size = 1.2, linetype = "dashed") +
  ggtitle("Pronóstico ARIMA1 vs IPC real (tramo test)") +
  xlab("Mes") + ylab("IPC") +
  theme_minimal()

p2 <- ggplot(df_comp, aes(x = Fecha)) +
  geom_line(aes(y = Real),  color = "blue",  size = 1.2) +
  geom_line(aes(y = SARIMA), color = "green", size = 1.2, linetype = "dashed") +
  ggtitle("Pronóstico SARIMA vs IPC real (tramo test)") +
  xlab("Mes") + ylab("IPC") +
  theme_minimal()

if (requireNamespace("gridExtra", quietly = TRUE)) {
  grid.arrange(p1, p2, ncol = 1)
}

library(zoo)
fechas_test <- as.yearmon(time(test_IPC))
pred_test_tbl <- data.frame(
  Anio      = as.integer(floor(fechas_test)),
  Mes       = as.integer(round(12 * (fechas_test - floor(fechas_test))) + 1),
  Fecha_YM  = format(fechas_test, "%Y-%m"),
  Observado = round(as.numeric(test_IPC), 3),
  ARIMA1    = round(as.numeric(fc_arima_test$mean[1:length(test_IPC)]), 3),
  ARIMA2    = round(as.numeric(fc_arima2_test$mean[1:length(test_IPC)]), 3),
  SARIMA    = round(as.numeric(fc_sarima_test$mean[1:length(test_IPC)]), 3),
  check.names = FALSE
)
cat("\n# === TEST desde", format(min(fechas_test), "%Y-%m"),
    "hasta", format(max(fechas_test), "%Y-%m"),
    "(", nrow(pred_test_tbl), "meses ) ===\n")
print(pred_test_tbl, row.names = FALSE)



# === 4) Gráfico con ZOOM 2017–2023 (comparando forecast vs test) ===
autoplot(window(IPC_sinO, start = c(2017,1))) +
  autolayer(FC_TEST$mean[1:length(test_IPC)], series = "Forecast (test)") +
  autolayer(test_IPC,                          series = "Observado (test)") +
  ggtitle("IPC — Zoom 2017–2023 (Modelo final)") +
  xlab("Tiempo") + ylab("IPC") +
  theme(legend.title = element_blank())
