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
X_train <- cbind(MS = train_MS, UR = train_UR, SMI = train_SMI)
X_test <- cbind(MS = test_MS, UR = test_UR, SMI = test_SMI)

X_train_est <- cbind(MS = train_MS_est, UR = train_UR_est, SMI = train_SMI_est)
X_test_est <- cbind(MS = test_MS_est, UR = test_UR_est, SMI = test_SMI_est)

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

# --- ARIMAX (con auto.arima) ---
# Entrenamiento con auto.arima + variables exógenas
modelo_PIB_ARIMAX_auto <- auto.arima(train_PIB,
                                     xreg = X_train,
                                     lambda = 0,         # para log-transform interna
                                     seasonal = FALSE,
                                     stepwise = FALSE,
                                     approximation = FALSE)

summary(modelo_PIB_ARIMAX_auto)
checkresiduals(modelo_PIB_ARIMAX_auto)

# Predicción sobre el test
pred_PIB_ARIMAX_auto <- forecast(modelo_PIB_ARIMAX_auto,
                                 xreg = X_test,
                                 h = length(test_PIB),
                                 biasadj = TRUE)

# Calcular métricas de precisión
accuracy_PIB_ARIMAX_auto <- accuracy(pred_PIB_ARIMAX_auto$mean, test_PIB)
print(accuracy_PIB_ARIMAX_auto)

# --- ARIMAX manual (con serie estacionaria) ---

# Comprobar longitudes despues de diferenciar las series
cat("length(train_PIB):", length(train_PIB), "\n")
cat("length(train_PIB_est):", length(train_PIB_est), "\n")
cat("length(train_MS_est):", length(train_MS_est), "\n")
cat("length(train_UR_est):", length(train_UR_est), "\n")
cat("length(train_SMI_est):", length(train_SMI_est), "\n")

modelo_PIB_ARIMAX_manual <- arima(train_PIB_est, order = c(3,0,3), xreg = X_train_est)
summary(modelo_PIB_ARIMAX_manual)
checkresiduals(modelo_PIB_ARIMAX_manual)

pred_ARIMAX_manual <- predict(modelo_PIB_ARIMAX_manual,
                              n.ahead = nrow(X_test_est),
                              newxreg = X_test_est)

last_values_log <- log(tail(train_PIB, 2))

pred_PIB_log_inv <- diffinv(pred_ARIMAX_manual$pred, differences = 2, xi = last_values_log)

pred_PIB_ARIMAX_manual <- exp(pred_PIB_log_inv[-c(1,2)])  # ahora en escala original

test_PIB_adj <- window(test_PIB, end = time(test_PIB)[length(pred_PIB_ARIMAX_manual)])

accuracy_PIB_ARIMAX_manual <- accuracy(pred_PIB_ARIMAX_manual, test_PIB_adj)
print(accuracy_PIB_ARIMAX_manual)


################################################################################
# 4. Comparación de modelos (añadimos ARIMAX_auto y ARIMAX_manual)
################################################################################
df_accuracy <- data.frame(
  Modelo = c("ARIMA_manual", "SARIMA", "AUTO.ARIMA", "ARIMAX_auto", "ARIMAX_manual"),
  RMSE   = c(accuracy_PIB_Arima["Test set","RMSE"],
             accuracy_PIB_sarima["Test set","RMSE"],
             accuracy_PIB_AutoArima["Test set","RMSE"],
             accuracy_PIB_ARIMAX_auto["Test set","RMSE"],
             accuracy_PIB_ARIMAX_manual["Test set","RMSE"]),
  MAE    = c(accuracy_PIB_Arima["Test set","MAE"],
             accuracy_PIB_sarima["Test set","MAE"],
             accuracy_PIB_AutoArima["Test set","MAE"],
             accuracy_PIB_ARIMAX_auto["Test set","MAE"],
             accuracy_PIB_ARIMAX_manual["Test set","MAE"]),
  MAPE   = c(accuracy_PIB_Arima["Test set","MAPE"],
             accuracy_PIB_sarima["Test set","MAPE"],
             accuracy_PIB_AutoArima["Test set","MAPE"],
             accuracy_PIB_ARIMAX_auto["Test set","MAPE"],
             accuracy_PIB_ARIMAX_manual["Test set","MAPE"])
)

print(df_accuracy)


################################################################################
# 6. Visualización (añadimos ARIMAX_auto y ARIMAX_manual)
################################################################################

# Dataframes para graficar
df_pred_arima        <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_revert_A)
df_pred_sarima       <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_sarima$mean)
df_pred_autoarima    <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_AutoArima$mean)
df_pred_arimax_auto  <- data.frame(Trimestre = time(test_PIB), Pred = pred_PIB_ARIMAX_auto$mean)
df_pred_arimax_manual <- data.frame(
  Trimestre = time(test_PIB_adj),
  Pred = as.numeric(pred_PIB_ARIMAX_manual)
)

# Serie completa
df_total <- data.frame(Trimestre = time(PIB_sinO), PIB = as.numeric(PIB_sinO))

# --- Gráficos ---
plot_arima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_arima, aes(x = Trimestre, y = Pred),
            color = 'red', linetype = 'dashed', linewidth = 1) +
  labs(title = 'ARIMA manual', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_sarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_sarima, aes(x = Trimestre, y = Pred),
            color = 'green', linetype = 'dashed', linewidth = 1) +
  labs(title = 'SARIMA', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_autoarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_autoarima, aes(x = Trimestre, y = Pred),
            color = 'black', linetype = 'dotdash', linewidth = 1) +
  labs(title = 'AUTO.ARIMA', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_arimax_auto <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_arimax_auto, aes(x = Trimestre, y = Pred),
            color = 'orange', linetype = 'dotted', linewidth = 1) +
  labs(title = 'ARIMAX (auto.arima)', y = 'PIB', x = 'Trimestre') + theme_minimal()

plot_arimax_manual <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_arimax_manual, aes(x = Trimestre, y = Pred),
            color = 'purple', linetype = 'dashed', linewidth = 1) +
  labs(title = 'ARIMAX (manual)', y = 'PIB', x = 'Trimestre') + theme_minimal()

# --- Visualización conjunta ---
grid.arrange(plot_arima, plot_sarima, plot_autoarima,
             plot_arimax_auto, plot_arimax_manual, ncol = 1)


################################################################################
# CROSS-VALIDATION (Rolling forecast) con los cinco modelos
################################################################################

# Parámetros
h <- 1                # pasos a predecir
train_size <- 20      # tamaño inicial del train
n <- length(PIB_sinO) # longitud de la serie

# Crear vectores para almacenar predicciones
pred_arima_manual_cv   <- rep(NA, n - train_size)
pred_sarima_cv         <- rep(NA, n - train_size)
pred_autoarima_cv      <- rep(NA, n - train_size)
pred_arimax_auto_cv    <- rep(NA, n - train_size)
pred_arimax_manual_cv  <- rep(NA, n - train_size)

# Rolling forecast
for(i in train_size:(n-1)) {
  train_PIB_cv <- window(PIB_sinO, end = time(PIB_sinO)[i])
  
  # Exógenas hasta ese punto
  train_MS_cv  <- window(MS_sinO, end = time(MS_sinO)[i])
  train_UR_cv  <- window(UR_sinO, end = time(UR_sinO)[i])
  train_SMI_cv <- window(SMI_sinO, end = time(SMI_sinO)[i])
  
  # Exógenas para el siguiente paso
  next_MS  <- MS_sinO[i+1]
  next_UR  <- UR_sinO[i+1]
  next_SMI <- SMI_sinO[i+1]
  
  X_train_cv <- cbind(MS = train_MS_cv, UR = train_UR_cv, SMI = train_SMI_cv)
  X_next_cv  <- matrix(c(next_MS, next_UR, next_SMI), nrow = 1)
  
  # --- ARIMA manual ---
  fit_manual <- try(arima(diff(log(train_PIB_cv), differences = 2), order = c(3,0,3)), silent = TRUE)
  if(!inherits(fit_manual, "try-error")) {
    fc_manual <- predict(fit_manual, n.ahead = h)
    last_vals <- log(tail(train_PIB_cv, 2))
    pred_arima_manual_cv[i - train_size + 1] <- exp(diffinv(fc_manual$pred, differences = 2, xi = last_vals)[3])
  }
  
  # --- SARIMA ---
  fit_sarima <- auto.arima(train_PIB_cv, seasonal = TRUE, lambda = 0)
  pred_sarima_cv[i - train_size + 1] <- as.numeric(forecast(fit_sarima, h = h)$mean)
  
  # --- AUTO.ARIMA ---
  fit_auto <- auto.arima(train_PIB_cv, seasonal = FALSE, lambda = 0)
  pred_autoarima_cv[i - train_size + 1] <- as.numeric(forecast(fit_auto, h = h)$mean)
  
  # --- ARIMAX (auto.arima) ---
  fit_arimax_auto <- try(auto.arima(train_PIB_cv,
                                    xreg = X_train_cv,
                                    lambda = 0,
                                    seasonal = FALSE,
                                    stepwise = FALSE,
                                    approximation = TRUE),
                         silent = TRUE)
  if(!inherits(fit_arimax_auto, "try-error")) {
    fc_arimax_auto <- forecast(fit_arimax_auto, xreg = X_next_cv, h = h, biasadj = TRUE)
    pred_arimax_auto_cv[i - train_size + 1] <- as.numeric(fc_arimax_auto$mean)
  }
  
  # --- ARIMAX manual (series diferenciadas) ---
  # Crear series estacionarias
  train_PIB_log_cv <- log(train_PIB_cv)
  train_PIB_est_cv <- diff(train_PIB_log_cv, differences = 2)
  train_MS_est_cv  <- diff(train_MS_cv, differences = 2)
  train_UR_est_cv  <- diff(train_UR_cv, differences = 2)
  train_SMI_est_cv <- diff(train_SMI_cv, differences = 2)
  
  # Matriz exógenas estacionarias
  X_train_est_cv <- cbind(MS = train_MS_est_cv, UR = train_UR_est_cv, SMI = train_SMI_est_cv)
  
  # Para el siguiente paso, usar las últimas diferencias (aproximación simple)
  X_next_est_cv <- matrix(c(
    tail(diff(train_MS_cv, differences = 2), 1),
    tail(diff(train_UR_cv, differences = 2), 1),
    tail(diff(train_SMI_cv, differences = 2), 1)
  ), nrow = 1)
  
  fit_arimax_manual <- try(arima(train_PIB_est_cv, order = c(3,0,3), xreg = X_train_est_cv), silent = TRUE)
  if(!inherits(fit_arimax_manual, "try-error")) {
    fc_arimax_manual <- predict(fit_arimax_manual, n.ahead = h, newxreg = X_next_est_cv)
    last_vals_log_cv <- log(tail(train_PIB_cv, 2))
    pred_log_inv_cv  <- diffinv(fc_arimax_manual$pred, differences = 2, xi = last_vals_log_cv)
    pred_arimax_manual_cv[i - train_size + 1] <- exp(pred_log_inv_cv[3])
  }
}

# Calcular errores
actual_cv <- window(PIB_sinO, start = time(PIB_sinO)[train_size + 1])
errors_arima_manual_cv  <- actual_cv - pred_arima_manual_cv
errors_sarima_cv        <- actual_cv - pred_sarima_cv
errors_autoarima_cv     <- actual_cv - pred_autoarima_cv
errors_arimax_auto_cv   <- actual_cv - pred_arimax_auto_cv
errors_arimax_manual_cv <- actual_cv - pred_arimax_manual_cv

# Función de métricas
metrics <- function(errors, actual) {
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae  <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

metrics_arima_manual_cv  <- metrics(errors_arima_manual_cv, actual_cv)
metrics_sarima_cv        <- metrics(errors_sarima_cv, actual_cv)
metrics_autoarima_cv     <- metrics(errors_autoarima_cv, actual_cv)
metrics_arimax_auto_cv   <- metrics(errors_arimax_auto_cv, actual_cv)
metrics_arimax_manual_cv <- metrics(errors_arimax_manual_cv, actual_cv)

# Tabla comparativa CV
df_metrics_cv <- data.frame(
  Modelo = c("ARIMA_manual_CV", "SARIMA_CV", "AUTO.ARIMA_CV",
             "ARIMAX_auto_CV", "ARIMAX_manual_CV"),
  RMSE = c(metrics_arima_manual_cv["RMSE"], metrics_sarima_cv["RMSE"],
           metrics_autoarima_cv["RMSE"], metrics_arimax_auto_cv["RMSE"],
           metrics_arimax_manual_cv["RMSE"]),
  MAE = c(metrics_arima_manual_cv["MAE"], metrics_sarima_cv["MAE"],
          metrics_autoarima_cv["MAE"], metrics_arimax_auto_cv["MAE"],
          metrics_arimax_manual_cv["MAE"]),
  MAPE = c(metrics_arima_manual_cv["MAPE"], metrics_sarima_cv["MAPE"],
           metrics_autoarima_cv["MAPE"], metrics_arimax_auto_cv["MAPE"],
           metrics_arimax_manual_cv["MAPE"])
)

print(df_metrics_cv)

################################################################################
# 7. Visualización de CV
################################################################################

start_index_cv <- train_size + 1
Trimestres_cv <- time(PIB_sinO)[start_index_cv:n]
Real_cv <- as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index_cv]))

df_cv_list <- list(
  ARIMA_manual = data.frame(Trimestre = Trimestres_cv, Real = Real_cv, Pred = pred_arima_manual_cv),
  SARIMA       = data.frame(Trimestre = Trimestres_cv, Real = Real_cv, Pred = pred_sarima_cv),
  AUTO.ARIMA   = data.frame(Trimestre = Trimestres_cv, Real = Real_cv, Pred = pred_autoarima_cv),
  ARIMAX_auto  = data.frame(Trimestre = Trimestres_cv, Real = Real_cv, Pred = pred_arimax_auto_cv),
  ARIMAX_manual = data.frame(Trimestre = Trimestres_cv, Real = Real_cv, Pred = pred_arimax_manual_cv)
)

plot_cv <- function(df, title, color) {
  ggplot(df, aes(x = Trimestre)) +
    geom_line(aes(y = Real), color = "black", size = 1) +
    geom_line(aes(y = Pred), color = color, linetype = "dashed", size = 1) +
    labs(title = paste0(title, " (Cross-Validation)"), y = "PIB", x = "Trimestre") +
    theme_minimal()
}

plot_arima_cv        <- plot_cv(df_cv_list$ARIMA_manual, "ARIMA manual", "red")
plot_sarima_cv       <- plot_cv(df_cv_list$SARIMA, "SARIMA", "green")
plot_autoarima_cv    <- plot_cv(df_cv_list$AUTO.ARIMA, "AUTO.ARIMA", "blue")
plot_arimax_auto_cv  <- plot_cv(df_cv_list$ARIMAX_auto, "ARIMAX (auto.arima)", "orange")
plot_arimax_manual_cv<- plot_cv(df_cv_list$ARIMAX_manual, "ARIMAX (manual)", "purple")

grid.arrange(plot_arima_cv, plot_sarima_cv, plot_autoarima_cv,
             plot_arimax_auto_cv, plot_arimax_manual_cv, ncol = 1)


