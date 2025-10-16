####### MODELADO DEL IPC (mensual) — ARIMA y SARIMA (con selección por diagnóstico)

library(dplyr)
library(forecast)
library(ggplot2)
library(fpp2)
library(tseries)

# -------------------- Datos -----------------
IPC_sinO <- readRDS("Datos/transformados/IPC_sinO_M.rds")
stopifnot(frequency(IPC_sinO) == 12)

# -------------------- Train / Test -----------------
train_IPC <- window(IPC_sinO, start = c(2000, 1), end = c(2021, 1))
test_IPC  <- window(IPC_sinO, start = c(2021, 2), end = c(2022, 9))
h <- length(test_IPC) + 2  # <- tus apuntes

# -------------------- 1) IDENTIFICACIÓN -----------------
lambda <- BoxCox.lambda(train_IPC)              # estabilización varianza SOLO si hace falta
d <- ndiffs(BoxCox(train_IPC, lambda))
D <- nsdiffs(BoxCox(train_IPC, lambda))
cat("Lambda:", round(lambda,3), " | d:", d, " | D:", D, "\n")
# -------------------- 2) ESTIMACIÓN -----------------
# ARIMA (no estacional) — candidato 1
fit_arima <- auto.arima(train_IPC,
                        lambda = lambda, biasadj = TRUE,
                        seasonal = FALSE,
                        stepwise = FALSE, approximation = FALSE,
                        d = d
)

# ARIMA (no estacional) — candidato 2, ampliando el espacio y sin drift
fit_arima2 <- auto.arima(train_IPC,
                         lambda = lambda, biasadj = TRUE,
                         seasonal = FALSE,
                         d = d, max.p = 6, max.q = 6,
                         allowdrift = FALSE,
                         stepwise = FALSE, approximation = FALSE
)

# SARIMA (estacional m=12)
fit_sarima <- auto.arima(train_IPC,
                         lambda = lambda, biasadj = TRUE,
                         seasonal = TRUE,
                         stepwise = FALSE, approximation = FALSE,
                         d = d, D = D
)

# -------------------- 3) DIAGNÓSTICO -----------------
p_arima   <- Box.test(residuals(fit_arima),   lag = 24, type = "Ljung-Box",
                      fitdf = length(coef(fit_arima)))$p.value
p_arima2  <- Box.test(residuals(fit_arima2),  lag = 24, type = "Ljung-Box",
                      fitdf = length(coef(fit_arima2)))$p.value
p_sarima  <- Box.test(residuals(fit_sarima),  lag = 24, type = "Ljung-Box",
                      fitdf = length(coef(fit_sarima)))$p.value

cat(sprintf("\nLjung-Box p-values -> ARIMA1: %.4f | ARIMA2: %.4f | SARIMA: %.4f\n",
            p_arima, p_arima2, p_sarima))
# -------------------- 4) PREDICCIÓN (sobre test; h = |test|+2) -----------------
h <- length(test_IPC)
fc_arima   <- forecast(fit_arima,  h = h)
fc_arima2  <- forecast(fit_arima2, h = h)
fc_sarima  <- forecast(fit_sarima, h = h)

# Usa SIEMPRE la fila "Test set"
acc_arima   <- accuracy(fc_arima,  test_IPC)["Test set", c("ME","RMSE","MAE","MAPE"), drop = FALSE]
acc_arima2  <- accuracy(fc_arima2, test_IPC)["Test set", c("ME","RMSE","MAE","MAPE"), drop = FALSE]
acc_sarima  <- accuracy(fc_sarima, test_IPC)["Test set", c("ME","RMSE","MAE","MAPE"), drop = FALSE]

acc_tab <- rbind(
  ARIMA1 = acc_arima,
  ARIMA2 = acc_arima2,
  SARIMA = acc_sarima
)
print(acc_tab)

# -------------------- 5) ELECCIÓN (solo modelos válidos) -----------------
valid <- c(ARIMA1 = p_arima  >= 0.05,
           ARIMA2 = p_arima2 >= 0.05,
           SARIMA = p_sarima >= 0.05)

# Aseguramos que acc_tab tiene nombres de fila
rownames(acc_tab) <- c("ARIMA1", "ARIMA2", "SARIMA")

# Si ninguno pasa Ljung-Box, entramos en else
if (any(valid)) {
  cand_names <- names(valid)[valid]
  # Comprobamos que existen en acc_tab
  cand_names <- intersect(cand_names, rownames(acc_tab))
  cand <- acc_tab[cand_names, , drop = FALSE]
  winner <- rownames(cand)[ which.min(cand[,"RMSE"]) ]
} else {
  warning("Ningún modelo pasa Ljung-Box; se elige el de menor RMSE provisionalmente.")
  winner <- rownames(acc_tab)[ which.min(acc_tab[,"RMSE"]) ]
}

cat("Modelo ganador:", winner, "\n")

# ===========================
# TABLA RESUMEN FINAL
# ===========================
resumen <- data.frame(
  Modelo = c("ARIMA1 log+diff","ARIMA2 log+diff","SARIMA log+diff"),
  RMSE   = c(accuracy(fc_arima,  test_IPC)["Test set","RMSE"],
             accuracy(fc_arima2, test_IPC)["Test set","RMSE"],
             accuracy(fc_sarima, test_IPC)["Test set","RMSE"]),
  MAE    = c(accuracy(fc_arima,  test_IPC)["Test set","MAE"],
             accuracy(fc_arima2, test_IPC)["Test set","MAE"],
             accuracy(fc_sarima, test_IPC)["Test set","MAE"]),
  MAPE   = c(accuracy(fc_arima,  test_IPC)["Test set","MAPE"],
             accuracy(fc_arima2, test_IPC)["Test set","MAPE"],
             accuracy(fc_sarima, test_IPC)["Test set","MAPE"]),
  LjungBox_p = c(p_arima, p_arima2, p_sarima)
)
resumen <- resumen[order(resumen$RMSE), ]
print(resumen, row.names = FALSE)


# =======================================
# DECISIÓN: MODELO SARIMA (0,1,0)(1,1,0)[12]
# (Transformación log + diff, pasa Ljung-Box, mejor diagnóstico y buen RMSE)
# =======================================
#
# Nota: reutilizamos 'lambda', 'd', 'D', 'IPC_sinO', 'train_IPC', 'test_IPC'
#       definidos antes. 'd=1', 'D=1' y periodicidad 12 (mensual).

# === 1) Reentrenar el modelo final con TODA la serie ===
MODELO_FINAL <- Arima(
  y      = IPC_sinO,
  order  = c(0, 1, 0),
  seasonal = list(order = c(1, 1, 0), period = 12),
  lambda = lambda,        # aplica log Box-Cox y revertirá al predecir
  biasadj = TRUE          # corrige sesgo al revertir
)


# === 2) Predicción sobre el tramo de test (para documentar accuracy) ===
# IMPORTANTE: el accuracy del test se calcula con un modelo entrenado SOLO en el train.
MODELO_TEST <- Arima(
  y = train_IPC,
  order = c(0, 1, 0),
  seasonal = list(order = c(1, 1, 0), period = 12),
  lambda = lambda,
  biasadj = TRUE
)

h_test <- length(test_IPC)
FC_TEST <- forecast(MODELO_TEST, h = h_test)

# Opción A (recomendada): pasar el objeto forecast y la serie test
ACC_FINAL <- accuracy(FC_TEST, test_IPC)[, c("ME","RMSE","MAE","MAPE")]
print(ACC_FINAL)


# Diagnóstico del modelo entrenado SOLO con datos 2000–2021
ggtsdisplay(residuals(MODELO_TEST))
checkresiduals(MODELO_TEST)


# === 3) Pronóstico FUTURO (12 meses) ===
FC_12 <- forecast(MODELO_FINAL, h = 12)

autoplot(FC_12) +
  coord_cartesian(xlim = c(2000, max(time(FC_12$mean)))) +
  ggtitle("IPC — Pronóstico 12 meses (Modelo final)") +
  xlab("Tiempo") + ylab("IPC_sinO")


# === 4) Gráfico con ZOOM 2017–2023 (comparando forecast vs test) ===
autoplot(window(IPC_sinO, start = c(2017,1))) +
  autolayer(FC_TEST$mean, series = "Forecast (test)") +
  autolayer(test_IPC,      series = "Observado (test)") +
  ggtitle("IPC — Zoom 2017–2023 (Modelo final)") +
  xlab("Tiempo") + ylab("IPC") +
  theme(legend.title = element_blank())



# =============================
# Gráfico comparativo final
# =============================

# Pronósticos del test
fc_arima_test  <- forecast(fit_arima,  h = length(test_IPC))
fc_sarima_test <- forecast(fit_sarima, h = length(test_IPC))

# Convertir a data frame para ggplot
df_comp <- data.frame(
  Fecha = as.numeric(time(test_IPC)),
  Real = as.numeric(test_IPC),
  ARIMA = as.numeric(fc_arima_test$mean),
  SARIMA = as.numeric(fc_sarima_test$mean)
)

library(ggplot2)
library(gridExtra)

p1 <- ggplot(df_comp, aes(x = Fecha)) +
  geom_line(aes(y = Real), color = "blue", size = 1.2) +
  geom_line(aes(y = ARIMA), color = "red", size = 1.2, linetype = "dashed") +
  ggtitle("Pronóstico ARIMA vs IPC real") +
  xlab("Mes") + ylab("IPC") +
  theme_minimal()

p2 <- ggplot(df_comp, aes(x = Fecha)) +
  geom_line(aes(y = Real), color = "blue", size = 1.2) +
  geom_line(aes(y = SARIMA), color = "green", size = 1.2, linetype = "dashed") +
  ggtitle("Pronóstico SARIMA vs IPC real") +
  xlab("Mes") + ylab("IPC") +
  theme_minimal()

# Mostrar ambos gráficos juntos
grid.arrange(p1, p2, ncol = 1)


