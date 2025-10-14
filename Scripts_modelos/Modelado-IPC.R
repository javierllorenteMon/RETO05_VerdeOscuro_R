### RETO 05, VERDE OSCURO, LABORAL KUTXA ###

####### MODELADO DE DATOS IPC

# Cargar librerias
library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)
library(tidyr)
#source("Scripts_preprocesamiento/Funciones.R")

# --- Datos SOLO IPC ---
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO_M.rds")
s <- frequency(IPC_sinO)

# ============================================================
# A) FLUJO CLÁSICO (TRABAJO EN SERIE TRANSFORMADA)
# ============================================================

# 1) Split en serie ORIGINAL (sin transformar)
raw_train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2021, 1))
raw_test_IPC  <- window(IPC_sinO, start = c(2021, 2), end = c(2022, 2))
h <- length(raw_test_IPC)

# 2) Elegir transformaciones SOLO con TRAIN (log / D / d)
use_log_IPC <- FALSE  # IPC no suele requerir log (varianza bastante estable)

y_tr <- if (use_log_IPC) log(raw_train_IPC) else raw_train_IPC
D_IPC <- if (s > 1) nsdiffs(y_tr) else 0
y_tr_s <- if (D_IPC > 0) diff(y_tr, lag = s, differences = D_IPC) else y_tr
d_IPC <- ndiffs(y_tr_s)

# Helper para aplicar SIEMPRE las mismas transformaciones
transform_with <- function(y, use_log, s, D, d) {
  z <- if (use_log) log(y) else y
  if (D > 0) z <- diff(z, lag = s, differences = D)
  if (d > 0) z <- diff(z, differences = d)
  z
}

# 3) Aplicar transformaciones a TODA la serie y volver a partir (estacionaria)
IPC_est_all <- transform_with(IPC_sinO, use_log_IPC, s, D_IPC, d_IPC)
offset <- D_IPC * s + d_IPC
time_est <- time(IPC_sinO)[(offset + 1):length(IPC_sinO)]
IPC_est_all <- ts(IPC_est_all, start = time_est[1], frequency = s)

train_IPC <- window(IPC_est_all, start = c(2000, 1), end = c(2021, 1))
test_IPC  <- window(IPC_est_all, start = c(2021, 2), end = c(2022, 2))

# 4) Modelos y evaluación (en espacio transformado)
# 4.1 Baseline: auto.arima NO estacional
fit_IPC_ns <- auto.arima(train_IPC, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
checkresiduals(fit_IPC_ns)
fc_IPC_ns  <- forecast(fit_IPC_ns, h = length(test_IPC))
acc_ns     <- accuracy(fc_IPC_ns, test_IPC)[, c("RMSE","MAE")]

# 4.2 SARIMA (estacional)
fit_IPC_s  <- auto.arima(train_IPC, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
checkresiduals(fit_IPC_s)
fc_IPC_s   <- forecast(fit_IPC_s, h = length(test_IPC))
acc_s      <- accuracy(fc_IPC_s, test_IPC)[, c("RMSE","MAE")]

# 4.3 ARIMA manual (1,0,0)
fit_IPC_arima <- Arima(train_IPC, order = c(1,0,0))
checkresiduals(fit_IPC_arima)
fc_IPC_arima  <- forecast(fit_IPC_arima, h = length(test_IPC))
acc_arima     <- accuracy(fc_IPC_arima, test_IPC)[, c("RMSE","MAE")]

# 5) Comparativa y gráfico (espacio transformado)
acc_table <- rbind(
  `auto.arima (no estacional)` = acc_ns,
  `SARIMA (seasonal=TRUE)`     = acc_s,
  `ARIMA(1,0,0)`               = acc_arima
)
print(round(acc_table, 4))  # RMSE/MAE en TEST; MAPE no aplica aquí

df_ipc <- tibble(
  Fecha = as.numeric(time(test_IPC)),
  Real  = as.numeric(test_IPC),
  `auto.arima (no estacional)` = as.numeric(fc_IPC_ns$mean),
  `SARIMA (seasonal=TRUE)`     = as.numeric(fc_IPC_s$mean),
  `ARIMA(1,0,0)`               = as.numeric(fc_IPC_arima$mean)
) |>
  pivot_longer(-Fecha, names_to = "Serie", values_to = "Valor")

ggplot(df_ipc, aes(Fecha, Valor, color = Serie, linetype = Serie)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Real"="black",
    "auto.arima (no estacional)"="darkorange",
    "SARIMA (seasonal=TRUE)"="red",
    "ARIMA(1,0,0)"="blue"
  )) +
  scale_linetype_manual(values = c(
    "Real"="solid",
    "auto.arima (no estacional)"="dashed",
    "SARIMA (seasonal=TRUE)"="dotdash",
    "ARIMA(1,0,0)"="solid"
  )) +
  labs(title = "IPC (transformado): comparación de modelos en test",
       x = "Año", y = "IPC (estacionario)", color = "", linetype = "") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------
# B) (OPCIONAL) PRESENTACIÓN EN ESCALA ORIGINAL (log->exp)
#    Ideal si quieres accuracy y gráficos “intuitivos” en valores originales
# ---------------------------------------------------------------

# Modelar en log(ORIGINAL) y volver con exp()
fit_log <- auto.arima(log(raw_train_IPC), seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
checkresiduals(fit_log)

fc_log  <- forecast(fit_log, h = h)
pred_OR <- exp(fc_log$mean)         # back-transform a escala original
acc_OR  <- accuracy(pred_OR, raw_test_IPC)[, c("RMSE","MAE","MAPE")]
print(round(acc_OR, 4))

# Gráfico en escala original (sin errores de colour)
autoplot(window(IPC_sinO, start = c(2019, 1))) +
  autolayer(window(IPC_sinO, start = time(raw_test_IPC)[1]),
            series = "Real (test)") +
  autolayer(ts(pred_OR, start = time(raw_test_IPC)[1], frequency = s),
            series = "Pred (log->exp)") +
  labs(title = "IPC (escala original): forecast con log->exp",
       x = "Año", y = "IPC (original)") +
  theme_minimal(base_size = 12)

c(
  no_estacional = Box.test(residuals(fit_IPC_ns),  type="Ljung-Box", lag=8)$p.value,
  SARIMA        = Box.test(residuals(fit_IPC_s),   type="Ljung-Box", lag=8)$p.value,
  ARIMA100      = Box.test(residuals(fit_IPC_arima), type="Ljung-Box", lag=8)$p.value
)


# ===============================================================
# CONCLUSIONES – IPC
#
# 1) Modelos en la serie TRANSFORMADA (estacionaria):
#    - auto.arima (no estacional)  → Ljung–Box p=0.264 (OK)
#    - ARIMA(1,0,0) manual         → Ljung–Box p=0.265 (OK)
#    - SARIMA(3,0,0)(0,0,1)[4]     → Ljung–Box p=0.477 (OK)
#
#    Rendimiento en TEST (RMSE / MAE):
#    - ARIMA(1,0,0)          ≈ 0.702 / 0.538   ← MEJOR
#    - auto.arima no estac.  ≈ 0.712 / 0.540   (muy similar)
#    - SARIMA                ≈ 0.819 / 0.770   (peor, sesgo a la baja)
#
#    Interpretación del gráfico (transformado):
#    - ARIMA(1,0,0) y el auto no estacional siguen bien la serie real.
#    - SARIMA se aleja claramente (pronósticos demasiado bajos).
#
#    Decisión en esta fase:
#    → Seleccionamos **ARIMA(1,0,0)** como modelo final para IPC en el
#      espacio transformado, por menor error en test y residuos ~ ruido blanco.
#
# 2) Presentación en ESCALA ORIGINAL (opcional, log → exp):
#    - Modelo seleccionado sobre log(IPC): ARIMA(2,1,2)(1,1,0)[4]
#    - Ljung–Box p=0.372 (OK). Métricas en test (escala original):
#         RMSE ≈ 1.71, MAE ≈ 1.42, MAPE ≈ 1.29%
#    - El gráfico en escala original muestra que el pronóstico captura
#      la subida final, aunque con cierta subestimación puntual.
#
# 3) Notas metodológicas:
#    - Las **transformaciones (D y d)** se deciden **solo con TRAIN** y se
#      aplican después a toda la serie para construir TRAIN/TEST transformados.
#    - En la serie transformada el **MAPE no es interpretable**; se comparan
#      modelos por **RMSE/MAE** y diagnóstico de residuos (Ljung–Box p>0.05).
#    - Para comunicar resultados en valores originales, modelar en **log(IPC)**
#      y volver con **exp()** es una opción limpia para obtener métricas
#      (RMSE/MAE/MAPE) en escala original.
#
# 4) Conclusión final (IPC):
#    - Modelo operativo (transformado): **ARIMA(1,0,0)**.
#    - Para informe en escala original, puede añadirse el forecast de log(IPC)
#      (exp back-transform), indicando sus métricas y que sirve solo a efectos
#      de presentación.
# ===============================================================


