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

# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO.rds")
MS_sinO <- readRDS("Datos/transformados/MS_sinO.rds")
UR_sinO <- readRDS("Datos/transformados/UR_sinO.rds")
SMI_sinO <- readRDS("Datos/transformados/SMI_sinO.rds")

# Descomposición de componentes
decomPIB <- decompose(PIB_sinO)
autoplot(decomPIB)
x<- decompose(PIB_TS)
autoplot(x)

decomIPC <- decompose(IPC_sinO)
autoplot(decomIPC)

decomMS <- decompose(MS_sinO)
autoplot(decomMS)

decomUR <- decompose(UR_sinO)
autoplot(decomUR)

decomSMI <- decompose(SMI_sinO)
autoplot(decomSMI)

# Comprobar estacionariedad con los test (seguramente sean no estacionarias porque no las hemos suavizado todavía)

# Definir funciones de cada test

# Test adf : sirve para detectar si queda tendencia estocástica.
test_adf <- function(serie){
  resultado <- adf.test(serie)
  pvalor <- resultado$p.value
  
  if(pvalor < 0.05){
    cat("=== Test ADF ===\n")
    cat("p-valor:", pvalor, "→ Serie ESTACIONARIA\n\n")
  } else {
    cat("=== Test ADF ===\n")
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
  } else {
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
    cat("p-valor:", pvalor, "→ Serie con autocorrelación (NO ruido blanco)\n\n")
  } else {
    cat("=== Test Ljung-Box ===\n")
    cat("p-valor:", pvalor, "→ Serie SIN autocorrelación (ruido blanco)\n\n")
  }
}

# Aplicar los test a las series temporales
test_adf(PIB_sinO)
test_kpss(PIB_sinO)
test_lb(PIB_sinO)

test_adf(IPC_sinO)
test_kpss(IPC_sinO)
test_lb(IPC_sinO)

test_adf(MS_sinO)
test_kpss(MS_sinO)
test_lb(MS_sinO)

test_adf(UR_sinO)
test_kpss(UR_sinO)
test_lb(UR_sinO)

test_adf(SMI_sinO)
test_kpss(SMI_sinO)
test_lb(SMI_sinO)


# Todas las series son NO estacionarias, asi que aplicamos diff y log para suavizarlas
test_adf(diff(log(PIB_sinO), lag = 4, differences = 2))
test_kpss(diff(log(PIB_sinO), lag = 4, differences = 2))
test_lb(diff(diff(diff(log(PIB_sinO)), lag = 4)))

tsdisplay(PIB_sinO)
tsdisplay(diff(log(PIB_sinO), lag = 4, differences = 2))

ndiffs(PIB_sinO)
nsdiffs(PIB_sinO)

# diff(diff(diff(log(PIB_sinO)), lag = 4))
# diff(log(PIB_sinO), lag = 4, differences = 4)

test_adf(diff(log(IPC_sinO), lag = 4, differences = 3))
test_kpss(diff(log(IPC_sinO), lag = 4, differences = 2))
test_lb(diff(diff(diff(log(IPC_sinO)), lag = 4)))

ndiffs(IPC_sinO)
nsdiffs(IPC_sinO)

test_adf(diff(log(MS_sinO), lag = 4, differences = 2))
test_kpss(diff(log(MS_sinO), lag = 4, differences = 2))
test_lb(diff(log(MS_sinO), lag = 4, differences = 20))

ndiffs(MS_sinO)
nsdiffs(MS_sinO)

tsdisplay(MS_sinO)
tsdisplay(diff(log(MS_sinO), lag = 4, differences = 2))


test_adf(diff(log(UR_sinO), lag = 4, differences = 2))
test_kpss(diff(log(UR_sinO), lag = 4, differences = 2))
test_lb(diff(UR_sinO, lag = 4, differences = 1))

tsdisplay(UR_sinO)
tsdisplay(diff(log(UR_sinO), lag = 4, differences = 2))

