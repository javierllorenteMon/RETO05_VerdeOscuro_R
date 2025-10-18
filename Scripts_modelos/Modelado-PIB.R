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

# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")

# Descomposición de componentes
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)

# Train(2000,1)(2021,2) test(2021,2)(2022,2)
train_PIB <- window(PIB_sinO, start = c(2000, 1), end = c(2020, 1))
test_PIB <- window(PIB_sinO, start = c(2020, 2), end = c(2022, 3))

# Comprobar estacionariedad con los test (seguramente sea no estacionaria porque no la hemos hecho estacionaria todavía)
test_estacionariedad(train_PIB, nombre = "PIB")

# Comprobar si tiene varianza creciente para ver si aplicarle log
ts.plot(train_PIB) # si parece tener varianza creciente, le aplicamos log 

# Ahora comprobar estacionalidad para ver si meterle lag en el diff()
# Con nsdiffs (que evalua si hace falta diferencia estacional) si da 1 tiene estacionalidad si da 0 no
nsdiffs(train_PIB) # si que da que tenga estacionalidad

# Tambien comprobamos la estacionalidad con acf (si tiene pico en lag = 4 hay estacionalidad) (si decae lentamente o no muestra patrón periódico, sin estacionalidad fuerte)
acf(train_PIB) # no tiene pico relevanre en lag = 4, asi que metemos probamos con lag y sin

# Test de estacionariedad otra vez (esta vez ajustando la serie)
tsdisplay(train_PIB)
train_PIB_est <- diff(log(train_PIB), differences = 2) # da estacionaria sin el lag
tsdisplay(train_PIB_est)
test_estacionariedad(train_PIB_est, nombre = "PIB")

# Modelos

# ------ AUTO.ARIMA (auto.arima con seasonal = FALSE) ------
modelo_PIB_AutoArima <- auto.arima(train_PIB, lambda = 0, seasonal = FALSE, stepwise=FALSE, approximation=FALSE) # el lambda = 0  ya aplica el log
summary(modelo_PIB_AutoArima)
checkresiduals(modelo_PIB_AutoArima)

pred_PIB_AutoArima <- forecast(modelo_PIB_AutoArima, h = length(test_PIB), biasadj = TRUE) #prediccion con correccion de sesgo

# Comparar con el test real
accuracy_PIB_AutoArima <- accuracy(pred_PIB_AutoArima$mean, test_PIB)
accuracy_PIB_AutoArima


# ------ ARIMA (manual) ------

# elegir p, d , q

acf(train_PIB_est) # mirar los picos para determinar p
pacf(train_PIB_est) # mirar los picos para determinar q
# d : si la serie ya es estacionaia porque ya hemos aplicado diffs ponemos 0

modelo_PIB_Arima <- arima(train_PIB_est, order = c(3, 0, 3))
summary(modelo_PIB_Arima)
checkresiduals(modelo_PIB_Arima)

pred_PIB_Arima <- forecast(modelo_PIB_Arima, h = length(test_PIB))

# Revertir las diferencias con diffinv
pred_PIB_log_A <- diffinv(pred_PIB_Arima$mean, 
                          differences = 2, 
                          xi = log(tail(train_PIB, 2)))

# Convertir de log a nivel original
pred_PIB_revert_A <- exp(pred_PIB_log_A)

# (Opcional) eliminar los dos primeros valores que son parte de xi
pred_PIB_revert_A <- pred_PIB_revert_A[-c(1:2)]

# Comparar con el test real
accuracy_PIB_Arima <- accuracy(pred_PIB_revert_A, test_PIB)
accuracy_PIB_Arima


# ------ SARIMA (auto.arima con seasonal = TRUE) ------
modelo_PIB_sarima <- auto.arima(train_PIB, seasonal = TRUE, lambda = 0)
summary(modelo_PIB_sarima)
checkresiduals(modelo_PIB_sarima)

pred_PIB_sarima <- forecast(modelo_PIB_sarima, h = length(test_PIB), biasadj = TRUE)

# Comparar con el test real
accuracy_PIB_sarima <- accuracy(pred_PIB_sarima, test_PIB)
accuracy_PIB_sarima


# Test lb para ver si es ruido blanco
interpretar_ljungbox <- function(modelo, lag = 24, tipo = "Ljung-Box") {
  test <- Box.test(residuals(modelo), lag = lag, type = tipo)
  p_valor <- test$p.value
  
  cat("Test de", tipo, "con lag =", lag, "\n")
  cat("Estadístico Q =", round(test$statistic, 3), 
      "| p-valor =", round(p_valor, 4), "\n")
  
  if (p_valor > 0.05) {
    cat("Los residuos parecen independientes (no autocorrelación significativa).\n")
  } else {
    cat("Los residuos muestran autocorrelación — el modelo puede mejorarse.\n")
  }
  
  invisible(test)
}

interpretar_ljungbox(modelo_PIB_Arima)
interpretar_ljungbox(modelo_PIB_sarima)
interpretar_ljungbox(modelo_PIB_AutoArima)

# Crear DataFrame comparativo de accuracys de los tres modelos

# Seleccionamos métricas principales: RMSE, MAE y MAPE
df_accuracy <- data.frame(
  Modelo = c("ARIMA_manual", "SARIMA", "AUTO.ARIMA"),
  RMSE = c(accuracy_PIB_Arima["Test set", "RMSE"],
           accuracy_PIB_sarima["Test set", "RMSE"],
           accuracy_PIB_AutoArima["Test set", "RMSE"]),
  MAE = c(accuracy_PIB_Arima["Test set", "MAE"],
          accuracy_PIB_sarima["Test set", "MAE"],
          accuracy_PIB_AutoArima["Test set", "MAE"]),
  MAPE = c(accuracy_PIB_Arima["Test set", "MAPE"],
           accuracy_PIB_sarima["Test set", "MAPE"],
           accuracy_PIB_AutoArima["Test set", "MAPE"])
)

print(df_accuracy)

# Graficar e interpretar resultados (auto.arima Arima y Sarima)

# Crear dataframe con toda la serie desde el 2000
df_total <- data.frame(
  Trimestre = time(window(PIB_sinO, start = c(2000, 1))),
  PIB = as.numeric(window(PIB_sinO, start = c(2000, 1)))
)

# --- ARIMA manual ---
df_pred_arima <- data.frame(
  Trimestre = time(test_PIB),
  Pred = as.numeric(pred_PIB_revert_A)
)

# --- SARIMA ---
df_pred_sarima <- data.frame(
  Trimestre = time(test_PIB),
  Pred = as.numeric(pred_PIB_sarima$mean)
)

# --- AUTO.ARIMA ---
df_pred_autoarima <- data.frame(
  Trimestre = time(test_PIB),
  Pred = as.numeric(pred_PIB_AutoArima$mean)
)

# Punto donde comienza el test (2021 Q3)
test_start <- start(test_PIB)[1] + (start(test_PIB)[2] - 1) / 4

# === ARIMA manual ===
plot_arima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_arima, aes(x = Trimestre, y = Pred), color = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(xintercept = test_start, color = "gray40", linetype = "dotted") +
  labs(title = 'ARIMA manual — Serie completa desde 2000 y pronóstico',
       y = 'PIB', x = 'Trimestre') +
  theme_minimal()

# === SARIMA ===
plot_sarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_sarima, aes(x = Trimestre, y = Pred), color = 'green', linetype = 'dashed', linewidth = 1) +
  geom_vline(xintercept = test_start, color = "gray40", linetype = "dotted") +
  labs(title = 'SARIMA — Serie completa desde 2000 y pronóstico',
       y = 'PIB', x = 'Trimestre') +
  theme_minimal()

# === AUTO.ARIMA ===
plot_autoarima <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = PIB), color = 'blue', linewidth = 1) +
  geom_line(data = df_pred_autoarima, aes(x = Trimestre, y = Pred), color = 'black', linetype = 'dotdash', linewidth = 1) +
  geom_vline(xintercept = test_start, color = "gray40", linetype = "dotted") +
  labs(title = 'AUTO.ARIMA — Serie completa desde 2000 y pronóstico',
       y = 'PIB', x = 'Trimestre') +
  theme_minimal()

# === Mostrar los tres gráficos juntos ===
grid.arrange(plot_arima, plot_sarima, plot_autoarima, ncol = 1)



############################################################################################
# CROSS - VALIDATION

# -----------
# Parámetros
# -----------
h <- 1                # pasos a predecir
train_size <- 40      # tamaño mínimo del train inicial
n <- length(PIB_sinO) # longitud total de la serie

# Crear vectores para almacenar predicciones
pred_arima_manual <- rep(NA, n - train_size)
pred_sarima       <- rep(NA, n - train_size)
pred_autoarima    <- rep(NA, n - train_size)

# -----------------
# Rolling forecast
# -----------------
for(i in train_size:(n-1)) {
  train <- window(PIB_sinO, end = time(PIB_sinO)[i])
  
  # --- ARIMA manual ---
  fit_manual <- try(arima(diff(log(train), differences = 2), order = c(3,0,3)), silent = TRUE)
  if(inherits(fit_manual, "try-error")) {
    pred_arima_manual[i - train_size + 1] <- NA
  } else {
    fc_manual <- predict(fit_manual, n.ahead = h)
    last_values <- log(tail(train, 2))
    pred_arima_manual[i - train_size + 1] <- exp(diffinv(fc_manual$pred, differences = 2, xi = last_values)[3])
  }
  # Revertir diferencias y log
  last_values <- log(tail(train, 2))
  pred_arima_manual[i - train_size + 1] <- exp(diffinv(fc_manual$pred, differences = 2, xi = last_values)[3])
  
  # --- SARIMA ---
  fit_sarima <- auto.arima(train, seasonal = TRUE, lambda = 0)
  pred_sarima[i - train_size + 1] <- as.numeric(forecast(fit_sarima, h = h)$mean)
  
  # --- AUTO.ARIMA ---
  fit_auto <- auto.arima(train, seasonal = FALSE, lambda = 0)
  pred_autoarima[i - train_size + 1] <- as.numeric(forecast(fit_auto, h = h)$mean)
}

# --------
# Errores
# --------
actual <- window(PIB_sinO, start = time(PIB_sinO)[train_size + 1])

errors_arima_manual <- actual - pred_arima_manual
errors_sarima       <- actual - pred_sarima
errors_autoarima    <- actual - pred_autoarima

# ---------
# Métricas
# ---------
metrics <- function(errors, actual) {
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae  <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

metrics_arima_manual <- metrics(errors_arima_manual, actual)
metrics_sarima       <- metrics(errors_sarima, actual)
metrics_autoarima    <- metrics(errors_autoarima, actual)


# ------------------------
# Crear tabla comparativa
# ------------------------
df_metrics <- data.frame(
  Modelo = c("ARIMA_manual", "SARIMA", "AUTO.ARIMA"),
  RMSE   = c(metrics_arima_manual["RMSE"], metrics_sarima["RMSE"], metrics_autoarima["RMSE"]),
  MAE    = c(metrics_arima_manual["MAE"], metrics_sarima["MAE"], metrics_autoarima["MAE"]),
  MAPE   = c(metrics_arima_manual["MAPE"], metrics_sarima["MAPE"], metrics_autoarima["MAPE"])
)

print(df_metrics)

# -----------------------
# Comprobar los residuos
# -----------------------
check_residuals <- function(errors, modelo_name) {
  cat("----", modelo_name, "----\n")
  print(Box.test(errors, lag = 12, type = "Ljung-Box"))
  print(adf.test(errors))
  print(kpss.test(errors))
  cat("\n")
}

check_residuals(errors_arima_manual, "ARIMA_manual") # correcto, pasa los 3 test
check_residuals(errors_sarima, "SARIMA") # correcto, pasa los 3 test
check_residuals(errors_autoarima, "AUTO.ARIMA") # no pasa los 3


# ---------
# Graficar
# ---------

# Dataframes individuales para cada modelo
df_arima_manual <- data.frame(
  Trimestre = time(PIB_sinO)[start_index:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index])),
  Pred      = pred_arima_manual
)

df_sarima <- data.frame(
  Trimestre = time(PIB_sinO)[start_index:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index])),
  Pred      = pred_sarima
)

df_autoarima <- data.frame(
  Trimestre = time(PIB_sinO)[start_index:n],
  Real      = as.numeric(window(PIB_sinO, start = time(PIB_sinO)[start_index])),
  Pred      = pred_autoarima
)

# --- Gráfica ARIMA manual ---
plot_arima <- ggplot(df_arima_manual, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "red", linetype = "dashed", size = 1) +
  labs(title = "ARIMA manual", y = "PIB", x = "Trimestre") +
  theme_minimal()

# --- Gráfica SARIMA ---
plot_sarima <- ggplot(df_sarima, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "green", linetype = "dashed", size = 1) +
  labs(title = "SARIMA", y = "PIB", x = "Trimestre") +
  theme_minimal()

# --- Gráfica AUTO.ARIMA ---
plot_autoarima <- ggplot(df_autoarima, aes(x = Trimestre)) +
  geom_line(aes(y = Real), color = "black", size = 1) +
  geom_line(aes(y = Pred), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "AUTO.ARIMA", y = "PIB", x = "Trimestre") +
  theme_minimal()

# --- Juntar los tres gráficos ---
grid.arrange(plot_arima, plot_sarima, plot_autoarima, ncol = 1)

