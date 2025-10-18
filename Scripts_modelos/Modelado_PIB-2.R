### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

################################################################################
# 0. Librerías y funciones
################################################################################
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)
source("Scripts_Preprocesamiento/Funciones.R")

################################################################################
# 1. Carga y preprocesamiento
################################################################################
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")

# Variables exogenas
MS_sinO <- readRDS("Datos/transformados/MS_sinO.rds")
UR_sinO <- readRDS("Datos/transformados/UR_sinO.rds")
SMI_sinO <- readRDS("Datos/transformados/SMI_sinO.rds")

# Descomposición de las series para ver tendencia, estacionalidad y residuales
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)

decomMS <- decompose(MS_sinO)
autoplot(decomMS)

decomUR <- decompose(UR_sinO)
autoplot(decomUR)

decomSMI <- decompose(SMI_sinO)
autoplot(decomSMI)

# División Train / Test
train_PIB <- window(PIB_sinO, start = c(2000,1), end = c(2020,1))
test_PIB  <- window(PIB_sinO, start = c(2020,2), end = c(2022,3))

train_MS  <- window(MS_sinO, start=c(2000,1), end=c(2020,1))
test_MS   <- window(MS_sinO, start=c(2020,2), end=c(2022,3))

train_UR  <- window(UR_sinO, start=c(2000,1), end=c(2020,1))
test_UR   <- window(UR_sinO, start=c(2020,2), end=c(2022,3))

train_SMI <- window(SMI_sinO, start=c(2000,1), end=c(2020,1))
test_SMI  <- window(SMI_sinO, start=c(2020,2), end=c(2022,3))

################################################################################
# 2. Análisis exploratorio y estacionariedad
################################################################################

# -----------------------------
# PIB
# -----------------------------

# Aplicar log si la serie tiene varianza creciente

# Comprobar varianza creciente
ts.plot(train_PIB) # si parece tener varianza creciente
train_PIB_log <- log(train_PIB)

# Comprobar estacionalidad
nsdiffs(train_PIB)
acf(train_PIB)

# Diferencias para estacionarizar
train_PIB_est <- diff(train_PIB_log, differences = 2)

# Comprobar estacionariedad
test_estacionariedad(train_PIB_est)

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
X_train <- cbind(MS = train_MS_est, UR = train_UR_est, SMI = train_SMI_est)
X_test <- cbind(MS = test_MS_est, UR = test_UR_est, SMI = test_SMI_est)

################################################################################
# 3. Modelado
################################################################################

# --- AUTO.ARIMA ---
modelo_PIB_AutoArima <- auto.arima(train_PIB, lambda = 0, seasonal = FALSE,
                                   stepwise = FALSE, approximation = FALSE)
checkresiduals(modelo_PIB_AutoArima)
pred_PIB_AutoArima <- forecast(modelo_PIB_AutoArima, h = length(test_PIB), biasadj = TRUE)
accuracy_PIB_AutoArima <- accuracy(pred_PIB_AutoArima$mean, test_PIB)

# --- ARIMA manual ---
modelo_PIB_Arima <- arima(train_PIB_est, order = c(3,0,3))
checkresiduals(modelo_PIB_Arima)
pred_PIB_Arima <- forecast(modelo_PIB_Arima, h = length(test_PIB))

pred_PIB_log_A <- diffinv(pred_PIB_Arima$mean, differences = 2, xi = log(tail(train_PIB,2)))
pred_PIB_revert_A <- exp(pred_PIB_log_A[-c(1,2)])
accuracy_PIB_Arima <- accuracy(pred_PIB_revert_A, test_PIB)

# --- SARIMA ---
modelo_PIB_sarima <- auto.arima(train_PIB, seasonal = TRUE, lambda = 0)
checkresiduals(modelo_PIB_sarima)
pred_PIB_sarima <- forecast(modelo_PIB_sarima, h = length(test_PIB), biasadj = TRUE)
accuracy_PIB_sarima <- accuracy(pred_PIB_sarima, test_PIB)

# --- ARIMAX ---
modelo_PIB_ARIMAX <- arima(train_PIB_est, 
                           order = c(3,0,3),          # mismo orden que tu ARIMA manual
                           xreg = X_train)
summary(modelo_PIB_ARIMAX)
checkresiduals(modelo_PIB_ARIMAX)
pred_ARIMAX <- predict(modelo_PIB_ARIMAX, 
                     n.ahead = nrow(X_test), 
                     newxreg = X_test)

# Revertir diferencias y log para obtener PIB en escala original
last_values <- log(tail(train_PIB, 2))  # dos últimos valores del train original
pred_PIB_ARIMAX <- exp(diffinv(pred_ARIMAX$pred, differences = 2, xi = last_values)[-(1:2)])

# Comparar con PIB real del test
test_PIB_adj <- window(test_PIB, end = time(test_PIB)[length(pred_PIB_ARIMAX)])
accuracy_PIB_ARIMAX <- accuracy(pred_PIB_ARIMAX, test_PIB_adj)
print(accuracy_PIB_ARIMAX)

################################################################################
# 4. Comparación de modelos
################################################################################
df_accuracy <- data.frame(
  Modelo = c("ARIMA_manual", "SARIMA", "AUTO.ARIMA"),
  RMSE   = c(accuracy_PIB_Arima["Test set","RMSE"],
             accuracy_PIB_sarima["Test set","RMSE"],
             accuracy_PIB_AutoArima["Test set","RMSE"]),
  MAE    = c(accuracy_PIB_Arima["Test set","MAE"],
             accuracy_PIB_sarima["Test set","MAE"],
             accuracy_PIB_AutoArima["Test set","MAE"]),
  MAPE   = c(accuracy_PIB_Arima["Test set","MAPE"],
             accuracy_PIB_sarima["Test set","MAPE"],
             accuracy_PIB_AutoArima["Test set","MAPE"])
)
print(df_accuracy)

################################################################################
# 5. Comprobación de residuos
################################################################################
check_residuals <- function(errors, modelo_name) {
  cat("----", modelo_name, "----\n")
  print(Box.test(errors, lag = 12, type = "Ljung-Box"))
  print(adf.test(errors))
  print(kpss.test(errors))
  cat("\n")
}

# Calcular residuos fuera de muestra (cross-validation)
errors_arima_manual <- test_PIB - pred_PIB_revert_A
errors_sarima       <- test_PIB - pred_PIB_sarima$mean
errors_autoarima    <- test_PIB - pred_PIB_AutoArima$mean

check_residuals(errors_arima_manual, "ARIMA_manual")
check_residuals(errors_sarima, "SARIMA")
check_residuals(errors_autoarima, "AUTO.ARIMA")

################################################################################
# 6. Visualización
################################################################################

# Dataframes para graficar
df_pred_arima <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_revert_A)
df_pred_sarima <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_sarima$mean)
df_pred_autoarima <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_AutoArima$mean)

df_total <- data.frame(Trimestre = time(PIB_sinO), PIB = as.numeric(PIB_sinO))

# Graficas
plot_arima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_arima, aes(x = Trimestre, y = Pred), color = 'red', linetype = 'dashed', linewidth = 1) +
  labs(title = 'ARIMA manual', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_sarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_sarima, aes(x = Trimestre, y = Pred), color = 'green', linetype = 'dashed', linewidth = 1) +
  labs(title = 'SARIMA', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_autoarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_autoarima, aes(x = Trimestre, y = Pred), color = 'black', linetype = 'dotdash', linewidth = 1) +
  labs(title = 'AUTO.ARIMA', y = 'PIB', x = 'Trimestre') + theme_minimal()

grid.arrange(plot_arima, plot_sarima, plot_autoarima, ncol = 1)


################################################################################
# CROSS-VALIDATION (Rolling forecast)
################################################################################

# Parámetros
h <- 1                # pasos a predecir
train_size <- 40      # tamaño inicial del train
n <- length(PIB_sinO) # longitud de la serie

# Crear vectores para almacenar predicciones
pred_arima_manual_cv <- rep(NA, n - train_size)
pred_sarima_cv       <- rep(NA, n - train_size)
pred_autoarima_cv    <- rep(NA, n - train_size)

# Rolling forecast
for(i in train_size:(n-1)) {
  train <- window(PIB_sinO, end = time(PIB_sinO)[i])
  
  # --- ARIMA manual ---
  fit_manual <- try(arima(diff(log(train), differences = 2), order = c(3,0,3)), silent = TRUE)
  if(!inherits(fit_manual, "try-error")) {
    fc_manual <- predict(fit_manual, n.ahead = h)
    last_values <- log(tail(train, 2))
    pred_arima_manual_cv[i - train_size + 1] <- exp(diffinv(fc_manual$pred, differences = 2, xi = last_values)[3])
  }
  
  # --- SARIMA ---
  fit_sarima <- auto.arima(train, seasonal = TRUE, lambda = 0)
  pred_sarima_cv[i - train_size + 1] <- as.numeric(forecast(fit_sarima, h = h)$mean)
  
  # --- AUTO.ARIMA ---
  fit_auto <- auto.arima(train, seasonal = FALSE, lambda = 0)
  pred_autoarima_cv[i - train_size + 1] <- as.numeric(forecast(fit_auto, h = h)$mean)
}

# Calcular errores
actual_cv <- window(PIB_sinO, start = time(PIB_sinO)[train_size + 1])
errors_arima_manual_cv <- actual_cv - pred_arima_manual_cv
errors_sarima_cv       <- actual_cv - pred_sarima_cv
errors_autoarima_cv    <- actual_cv - pred_autoarima_cv

# Función para métricas
metrics <- function(errors, actual) {
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae  <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

metrics_arima_manual_cv <- metrics(errors_arima_manual_cv, actual_cv)
metrics_sarima_cv       <- metrics(errors_sarima_cv, actual_cv)
metrics_autoarima_cv    <- metrics(errors_autoarima_cv, actual_cv)

# Crear tabla comparativa CV
df_metrics_cv <- data.frame(
  Modelo = c("ARIMA_manual_CV", "SARIMA_CV", "AUTO.ARIMA_CV"),
  RMSE   = c(metrics_arima_manual_cv["RMSE"], metrics_sarima_cv["RMSE"], metrics_autoarima_cv["RMSE"]),
  MAE    = c(metrics_arima_manual_cv["MAE"], metrics_sarima_cv["MAE"], metrics_autoarima_cv["MAE"]),
  MAPE   = c(metrics_arima_manual_cv["MAPE"], metrics_sarima_cv["MAPE"], metrics_autoarima_cv["MAPE"])
)

print(df_metrics_cv)

# Comprobar residuos (ruido blanco)
check_residuals_cv <- function(errors, modelo_name) {
  cat("----", modelo_name, "----\n")
  print(Box.test(errors, lag = 12, type = "Ljung-Box"))
  print(adf.test(errors))
  print(kpss.test(errors))
  cat("\n")
}

check_residuals_cv(errors_arima_manual_cv, "ARIMA_manual_CV")
check_residuals_cv(errors_sarima_cv, "SARIMA_CV")
check_residuals_cv(errors_autoarima_cv, "AUTO.ARIMA_CV")

# Graficar predicciones vs reales
start_index_cv <- train_size + 1
df_arima_manual_cv <- data.frame(
  Trimestre = time(PIB_sinO)[start_index_cv:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index_cv])),
  Pred      = pred_arima_manual_cv
)

df_sarima_cv <- data.frame(
  Trimestre = time(PIB_sinO)[start_index_cv:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index_cv])),
  Pred      = pred_sarima_cv
)

df_autoarima_cv <- data.frame(
  Trimestre = time(PIB_sinO)[start_index_cv:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index_cv])),
  Pred      = pred_autoarima_cv
)

plot_arima_cv <- ggplot(df_arima_manual_cv, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "red", linetype = "dashed", size = 1) +
  labs(title = "ARIMA manual CV", y = "PIB", x = "Trimestre") +
  theme_minimal()

plot_sarima_cv <- ggplot(df_sarima_cv, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "green", linetype = "dashed", size = 1) +
  labs(title = "SARIMA CV", y = "PIB", x = "Trimestre") +
  theme_minimal()

plot_autoarima_cv <- ggplot(df_autoarima_cv, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "AUTO.ARIMA CV", y = "PIB", x = "Trimestre") +
  theme_minimal()

grid.arrange(plot_arima_cv, plot_sarima_cv, plot_autoarima_cv, ncol = 1)




