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
source("Scripts_preprocesamiento/Funciones.R")

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





#IPC
#Train(2000,1)(2021,2) test(2021,2)(2022,2)
train_IPC <- window(IPC_est, start = c(2000, 1), end = c(2021, 1))
test_IPC <- window(IPC_est, start = c(2021, 2), end = c(2022, 2))

# auto.arima 
fit_IPC <- auto.arima(train_IPC, seasonal = FALSE)
checkresiduals(fit_IPC)                # ya pasa
fc_IPC  <- forecast(fit_IPC, h = length(test_IPC))
accuracy(fc_IPC, test_IPC)[, c("RMSE","MAE")]

# Modelo SARIMA (ya lo tienes)
fit_IPC_s <- auto.arima(train_IPC, seasonal = TRUE,
                        stepwise = TRUE, approximation = FALSE)
checkresiduals(fit_IPC_s)   # p-value > 0.05 = OK

# Pronóstico y métricas en TEST
fc_IPC_s <- forecast(fit_IPC_s, h = length(test_IPC))
acc_s    <- accuracy(fc_IPC_s, test_IPC)

# Extraer AICc SIN la función AICc()
aicc_s   <- fit_IPC_s$aicc

# Resumen limpio
cat(paste(capture.output(fit_IPC_s), collapse = "\n"), "\n")
cat("AICc:", aicc_s, "\n")
cat("RMSE test:", acc_s["Test set","RMSE"], " | MAE test:", acc_s["Test set","MAE"], "\n")

# CONCLUSIONES
# Para IPC (serie transformada IPC_est), el modelo no estacional ARIMA(1,0,0) presenta residuos tipo 
# ruido blanco (Ljung–Box p=0.264) y mejores errores en test (RMSE=0.712; MAE=0.540) que el modelo 
# estacional ARIMA(3,0,0)(0,0,1)[4] (RMSE=0.819; MAE=0.770), aunque este último mejora el AICc en 
# entrenamiento. Por tanto, seleccionamos ARIMA(1,0,0) como modelo final en esta fase.



# ARIMA IPC
fit_IPC_arima <- Arima(train_IPC, order = c(1,0,0))
summary(fit_IPC_arima)

checkresiduals(fit_IPC_arima)

pred_IPC_arima <- forecast(fit_IPC_arima, h = length(test_IPC))
accuracy(pred_IPC_arima, test_IPC)[, c("RMSE", "MAE")]

autoplot(pred_IPC_arima) +
  autolayer(test_IPC, series = "Test", color = "red") +
  ggtitle("Pronóstico IPC - Modelo ARIMA(1,0,0)") +
  xlab("Tiempo") + ylab("IPC Estacionario")

# ---------------------------------------------------------------
# CONCLUSIÓN MODELO ARIMA(1,0,0) - IPC
#
# El modelo ARIMA(1,0,0) con media distinta de cero ha mostrado
# un buen ajuste para la serie estacionaria del IPC.
#
# ✔️ El test de Ljung-Box (p-value = 0.26) confirma que los residuos
#    se comportan como ruido blanco → el modelo es adecuado.
# ✔️ No se detecta autocorrelación significativa en los residuos.
# ✔️ Los errores de predicción son aceptables:
#      RMSE (train) ≈ 0.43 | MAE (train) ≈ 0.31
#      RMSE (test)  ≈ 0.70 | MAE (test)  ≈ 0.54
#
# 🔹 Interpretación:
#   El modelo indica una ligera autocorrelación positiva (AR(1)=0.31),
#   lo que sugiere que el valor actual del IPC depende moderadamente
#   del valor anterior. No hay evidencia de estacionalidad en esta fase.
#
# En resumen:
# → El ARIMA(1,0,0) explica correctamente la dinámica no estacional del IPC
#   y sirve como base sólida antes de aplicar modelos SARIMA con estacionalidad.
# ---------------------------------------------------------------

# Evaluación técnica del gráfico:
# El modelo ARIMA(1,0,0) presenta predicciones estables y sin sobreajuste.
# Las observaciones reales (línea roja) caen dentro del intervalo de confianza,
# lo que confirma un buen comportamiento predictivo y residuos tipo ruido blanco.
# El modelo tiende a suavizar los picos, pero mantiene la tendencia general del IPC.

