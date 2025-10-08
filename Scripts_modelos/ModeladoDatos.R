### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DE DATOS

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(gridExtra)

# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO.rds")
MS_sinO <- readRDS("Datos/transformados/MS_sinO.rds")
UR_sinO <- readRDS("Datos/transformados/UR_sinO.rds")
SMI_sinO <- readRDS("Datos/transformados/SMI_sinO.rds")

# Descomposición de componentes
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)

decomIPC <- decompose(IPC_sinO)
autoplot(decomIPC)

decomMS <- decompose(MS_sinO)
autoplot(decomMS)

decomUR <- decompose(UR_sinO)
autoplot(decomUR)

decomSMI <- decompose(SMI_sinO)
autoplot(decomSMI)

# Comprobar estacionariedad con los test (seguramente sean no estacionarias porque no las hemos suavizado todavía)

# Test adf : sirve para detectar si queda tendencia estocástica.
test_adf <- function(serie){ 
  resultado <- adf.test(serie) 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test ADF ===\n") 
    cat("p-valor:", pvalor, "→ Serie ESTACIONARIA\n\n") } 
  else { cat("=== Test ADF ===\n") 
    cat("p-valor:", pvalor, "→ Serie NO estacionaria\n\n") 
  } 
}

# Test kpss : lo contrario a adf, dice si queda tendencia determinista o varianza no constante.
test_kpss <- function(serie){ 
  resultado <- kpss.test(serie, null="Level") 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test KPSS ===\n") 
    cat("p-valor:", pvalor, "→ Serie NO estacionaria\n\n") 
  } 
  else { 
    cat("=== Test KPSS ===\n") 
    cat("p-valor:", pvalor, "→ Serie ESTACIONARIA\n\n") 
  } 
}

# Test LB : dice si los residuos de un modelo se comportan como ruido blanco, se usa despues de ajustar un modelo.
test_lb <- function(serie, lags=12){ 
  resultado <- Box.test(serie, lag=lags, type="Ljung-Box") 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test Ljung-Box ===\n") 
    cat("p-valor:", pvalor, "→ Serie con autocorrelación (NO ruido blanco)\n\n") } 
  else { 
    cat("=== Test Ljung-Box ===\n") 
    cat("p-valor:", pvalor, "→ Serie SIN autocorrelación (ruido blanco)\n\n") 
  }
}

# Juntar todos los test en una sola funcion
test_estacionariedad <- function(serie, nombre="Serie") {
  cat("=== ", nombre, " ===\n")
  test_adf(serie)
  test_kpss(serie)
  test_lb(serie)
  cat("\n---------------------------\n")
}

# Aplicar los test a las series temporales
test_estacionariedad(PIB_sinO, nombre = "PIB")

test_estacionariedad(IPC_sinO, nombre = "IPC")

test_estacionariedad(MS_sinO, nombre = "MS")

test_estacionariedad(UR_sinO, nombre = "UR")

test_estacionariedad(SMI_sinO, nombre = "SMI")

# Todas las series son NO estacionarias (damos por hecho que son estacionarias cuando pasan al menos 2 tests), asi que aplicamos diff y log para hacerlas estacionarias

# Primero comprobar cuales tienen varianza creciente para aplicarles log a esas (a las otras solo en caso de que no se hagan estacionarias con diff)
ts.plot(PIB_sinO) # si parece tener varianza creciente
ts.plot(IPC_sinO) # tambien
ts.plot(MS_sinO) # no
ts.plot(UR_sinO) # no
ts.plot(SMI_sinO) # no

# Ahora comprobar estacionalidad para ver a cuales meterles lag en diff
# Con nsdiffs (que evalua si hace falta diferencia estacional) si da 1 tiene estacionalidad si da 0 no
nsdiffs(PIB_sinO)
nsdiffs(IPC_sinO)
nsdiffs(MS_sinO)
nsdiffs(UR_sinO)
nsdiffs(SMI_sinO) # no hay estacionalidad

# Tambien comprobamos la estacionalidad con acf (si tiene pico en lag = 4 hay estacionalidad) (si decae lentamente o no muestra patrón periódico, sin estacionalidad fuerte)
acf(PIB_sinO)
acf(IPC_sinO)
acf(MS_sinO)
acf(UR_sinO) # no tiene estacionalidad
acf(SMI_sinO) # no tiene estacionalidad

# Test estacionariedad (si a las series que tienen estacinoalidad, al meter lag los test piden mas diferencias, no metemos lag)
tsdisplay(PIB_sinO)
PIB_est <- diff(log(PIB_sinO), differences = 1)
tsdisplay(PIB_est)
test_estacionariedad(PIB_est, nombre = "PIB")

tsdisplay(IPC_sinO)
IPC_est <- diff(diff(IPC_sinO), lag = 4)
tsdisplay(IPC_est)
test_estacionariedad(IPC_est, nombre = "IPC")

tsdisplay(MS_sinO)
MS_est <- diff(log(MS_sinO), differences = 1)
tsdisplay(MS_est)
test_estacionariedad(MS_est, nombre = "MS")

tsdisplay(UR_sinO)
UR_est <- diff(UR_sinO, differences = 2)
tsdisplay(UR_est)
test_estacionariedad(UR_est, nombre = "UR")

tsdisplay(SMI_sinO)
SMI_est <- diff(SMI_sinO, differences = 1)
tsdisplay(SMI_est)
test_estacionariedad(SMI_est, nombre = "SMI")

# Modelado

# PIB
# Train(2000,1)(2021,2) test(2021,2)(2022,2)
train_PIB <- window(PIB_est, start = c(2000, 1), end = c(2021, 1))
test_PIB <- window(PIB_est, start = c(2021, 2), end = c(2022, 2))

# ARIMA
modelo_PIB_Arima <- auto.arima(train_PIB, seasonal = FALSE)
summary(modelo_PIB_Arima)
checkresiduals(modelo_PIB_Arima)

pred_PIB_Arima <- forecast(modelo_PIB_Arima, h = length(test_PIB))

accuracy_PIB_Arima <- accuracy(pred_PIB_Arima, test_PIB)

# SARIMA
modelo_PIB_sarima <- auto.arima(train_PIB, seasonal = TRUE)
summary(modelo_PIB_sarima)
checkresiduals(modelo_PIB_sarima)

pred_PIB_sarima <- forecast(modelo_PIB_sarima, h = length(test_PIB))

accuracy_PIB_sarima <- accuracy(pred_PIB_sarima, test_PIB)

# Interpretar accuracys

# Crear tabla resumen de métricas principales
comparacion_PIB <- data.frame(
  Modelo = c("ARIMA", "SARIMA"),
  RMSE = c(accuracy_PIB_Arima["Test set", "RMSE"],
           accuracy_PIB_sarima["Test set", "RMSE"]),
  MAE = c(accuracy_PIB_Arima["Test set", "MAE"],
          accuracy_PIB_sarima["Test set", "MAE"]),
  MAPE = c(accuracy_PIB_Arima["Test set", "MAPE"],
           accuracy_PIB_sarima["Test set", "MAPE"]),
  TheilsU = c(accuracy_PIB_Arima["Test set", "Theil's U"],
              accuracy_PIB_sarima["Test set", "Theil's U"])
)

print(comparacion_PIB)

# --- Gráfico de comparación de predicciones ---
# Convertir a data.frame
df_pred <- data.frame(
  Fecha = time(test_PIB),
  Real = as.numeric(test_PIB),
  ARIMA = as.numeric(pred_PIB_Arima$mean),
  SARIMA = as.numeric(pred_PIB_sarima$mean)
)

# Graficar valores reales y predicciones
grafico_pred <- ggplot(df_pred, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), linewidth = 1.2) +
  geom_line(aes(y = ARIMA, color = "ARIMA"), linetype = "dashed", size = 1) +
  geom_line(aes(y = SARIMA, color = "SARIMA"), linetype = "dotdash", size = 1) +
  labs(
    title = "Comparación de modelos ARIMA vs SARIMA - PIB (Test Set)",
    x = "Año",
    y = "PIB (diferenciado/transformado)",
    color = "Serie"
  ) +
  scale_color_manual(values = c("Real" = "black", "ARIMA" = "blue", "SARIMA" = "red")) +
  theme_minimal()

print(grafico_pred)

