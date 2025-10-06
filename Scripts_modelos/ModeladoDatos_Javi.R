### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DE DATOS

library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)


# Cargar ficheros
PIB_sinO <- readRDS("Datos/transformados/PIB_sinO.rds")
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO.rds")
MS_sinO  <- readRDS("Datos/transformados/MS_sinO.rds")
UR_sinO  <- readRDS("Datos/transformados/UR_sinO.rds")
SMI_sinO <- readRDS("Datos/transformados/SMI_sinO.rds")

# Test
test_adf <- function(serie){
  r <- adf.test(serie)
  cat("ADF p-valor:", format(r$p.value, digits=3),
      ifelse(r$p.value < 0.05, "→ ESTACIONARIA\n", "→ NO estacionaria\n"))
}
test_kpss <- function(serie){
  r <- kpss.test(serie, null = "Level")
  cat("KPSS p-valor:", format(r$p.value, digits=3),
      ifelse(r$p.value < 0.05, "→ NO estacionaria\n", "→ ESTACIONARIA\n"))
}

# Función 
# - Aplica log si se indica y los valores son > 0
# - Recomienda d y D con ndiffs/nsdiffs
# - Aplica diferencias: primero estacional (si s>1 y D>0), luego no estacional (d>0)
# - Devuelve lista con todo
transformar_y_diagnosticar <- function(x_ts, use_log = FALSE, nombre = deparse(substitute(x_ts))) {
  cat("\n========================\nSerie:", nombre, "\n========================\n")
  s <- frequency(x_ts)
  cat("Frecuencia detectada (s):", s, "\n")

  # Verificación inicial
  cat("\n-- Tests en la serie original --\n")
  try(test_adf(x_ts))
  try(test_kpss(x_ts))

  # Log si procede
  y <- x_ts
  applied_log <- FALSE
  if (use_log) {
    if (all(y > 0, na.rm = TRUE)) {
      y <- log(y)
      applied_log <- TRUE
      cat("\nAplicado log(): sí (varianza creciente y valores > 0)\n")
    } else {
      cat("\nAplicado log(): NO (hay valores <= 0). Se omite.\n")
    }
  } else {
    cat("\nAplicado log(): no (no se considera varianza creciente clara)\n")
  }

  # Recomendar d y D
  rec_d  <- suppressWarnings(ndiffs(y))     # diferencias normales
  rec_D  <- if (s > 1) suppressWarnings(nsdiffs(y)) else 0  # estacionales si s>1
  cat("Recomendación ndiffs (d):", rec_d, "\n")
  cat("Recomendación nsdiffs (D):", rec_D, "\n")

  # Aplicar diferencias: primero estacional (si procede), luego normal
  y_tr <- y
  if (s > 1 && rec_D > 0) {
    y_tr <- diff(y_tr, lag = s, differences = rec_D)
    cat("Diferencia estacional aplicada: lag =", s, ", D =", rec_D, "\n")
  } else {
    cat("Diferencia estacional aplicada: no\n")
  }
  if (rec_d > 0) {
    y_tr <- diff(y_tr, differences = rec_d)
    cat("Diferencia no estacional aplicada: d =", rec_d, "\n")
  } else {
    cat("Diferencia no estacional aplicada: no\n")
  }

  # Tests tras la transformación
  cat("\n-- Tests tras transformación --\n")
  try(test_adf(y_tr))
  try(test_kpss(y_tr))

  # ACF/PACF para decidir p,q(P,Q)
  cat("\nMostrando tsdisplay de la serie transformada (ACF/PACF)...\n")
  suppressWarnings(forecast::tsdisplay(y_tr, main = paste0(nombre, " (transformada)")))

  # Ajuste ARIMA “rápido” (auto.arima) como referencia
  cat("\nAjustando auto.arima sobre la serie (con log si aplicado y sin diferencias previas explícitas)...\n")
  # Nota: auto.arima decidirá d/D internamente; esto es solo para tener un modelo base.
  fit <- tryCatch({
    auto.arima(y, seasonal = (s > 1), stepwise = TRUE, approximation = FALSE,
               allowmean = TRUE, allowdrift = TRUE, allowseasonal = TRUE)
  }, error = function(e) NULL)

  if (!is.null(fit)) {
    cat("Modelo auto.arima sugerido:", capture.output(fit), sep = "\n"); cat("\n")
    # Diagnóstico residuos
    cat("Ljung-Box en residuos (lag = 2*s o 12 si s<=1):\n")
    lag_lb <- ifelse(s > 1, 2*s, 12)
    print(Box.test(residuals(fit), type = "Ljung-Box", lag = lag_lb))
  } else {
    cat("auto.arima falló en el ajuste; revisa la serie.\n")
  }

  # Devolver resultados
  list(
    name = nombre,
    s = s,
    applied_log = applied_log,
    rec_d = rec_d,
    rec_D = rec_D,
    y_original = x_ts,
    y_logged = if (applied_log) y else NULL,
    y_transformed = y_tr,
    fit = fit
  )
}

# Tu clasificación por varianza creciente 
# Varianza creciente: PIB, MS  → usar log
# No evidente varianza creciente: IPC, UR, SMI → NO log (aunque podría probarse luego en IPC/SMI)
res_PIB <- transformar_y_diagnosticar(PIB_sinO, use_log = TRUE,  nombre = "PIB")
res_MS  <- transformar_y_diagnosticar(MS_sinO,  use_log = TRUE,  nombre = "MS")
res_IPC <- transformar_y_diagnosticar(IPC_sinO, use_log = FALSE, nombre = "IPC")
res_UR  <- transformar_y_diagnosticar(UR_sinO,  use_log = FALSE, nombre = "UR")
res_SMI <- transformar_y_diagnosticar(SMI_sinO, use_log = FALSE, nombre = "SMI")

# (Opcional) Ajuste ARIMA con órdenes forzados a partir de recomendaciones 

# Ejemplo para PIB usando rec_d y rec_D detectados (si quieres “congelar” d y D)
ajustar_arima_con_ordenes <- function(res) {
  y <- if (isTRUE(res$applied_log)) log(res$y_original) else res$y_original
  s <- res$s
  fit_forzado <- tryCatch({
    auto.arima(
      y,
      d = res$rec_d,
      D = res$rec_D,
      seasonal = (s > 1),
      stepwise = TRUE,
      approximation = FALSE
    )
  }, error = function(e) NULL)
  if (!is.null(fit_forzado)) {
    cat("\n[", res$name, "] ARIMA forzado con d=", res$rec_d, " D=", res$rec_D, ":\n", sep = "")
    print(fit_forzado)
    cat("\nLjung-Box en residuos:\n")
    lag_lb <- ifelse(s > 1, 2*s, 12)
    print(Box.test(residuals(fit_forzado), type = "Ljung-Box", lag = lag_lb))
  } else {
    cat("\n[", res$name, "] No se pudo ajustar ARIMA forzado.\n", sep = "")
  }
  invisible(fit_forzado)
}


# fit_PIB_forzado <- ajustar_arima_con_ordenes(res_PIB)
# fit_MS_forzado  <- ajustar_arima_con_ordenes(res_MS)
# fit_IPC_forzado <- ajustar_arima_con_ordenes(res_IPC)
# fit_UR_forzado  <- ajustar_arima_con_ordenes(res_UR)
# fit_SMI_forzado <- ajustar_arima_con_ordenes(res_SMI)
