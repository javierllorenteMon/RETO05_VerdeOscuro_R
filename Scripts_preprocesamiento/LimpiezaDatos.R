library(dplyr)
library(openxlsx)
library(naniar)
library(forecast)
library(ggplot2)
Pib_Paises <- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
Exogenas    <- read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

# --- Filtro ITA + limpieza básica
datos1 <- Pib_Paises %>% filter(Code == "ITA")
datos2 <- Exogenas    %>% filter(Code == "ITA")

vis_miss(datos1, cluster = TRUE)
vis_miss(datos2, cluster = TRUE)
miss_var_summary(datos1)

# SERIES TEMPORALES CON PIB PAISES
Datos1 <- datos1 %>%
  filter(!is.na(GDP.billion.currency.units)) %>%
  arrange(Year, Month)   # <- importante

vis_miss(Datos1, cluster = TRUE)

# # --- Chequeo de regularidad (opcional pero útil)
# stopifnot(all(Datos1$Month %in% c(3,6,9,12)))
# stopifnot(all(table(Datos1$Year) == 4))

# --- Calcular inicio automáticamente
q_start <- Datos1$Month[1] / 3  
start_ts <- c(Datos1$Year[1], q_start)

# --- Series univariantes
GDP_TS <- ts(Datos1$GDP.billion.currency.units, start = start_ts, frequency = 4)
CPI_TS <- ts(Datos1$Consumer.Price.Index..CPI.,  start = start_ts, frequency = 4)

class(GDP_TS); class(CPI_TS)
autoplot(GDP_TS)
autoplot(CPI_TS)

# --- Serie multivariante (PIB + CPI)
y_multi <- cbind(
  PIB = Datos1$GDP.billion.currency.units,
  CPI = Datos1$Consumer.Price.Index..CPI.
)

TS_MULTI <- ts(y_multi, start = start_ts, frequency = 4)
class(TS_MULTI)   # "mts" "ts" "matrix" "array"
autoplot(TS_MULTI)



# # --- Calcular inicio automáticamente (EXÓGENAS)
# Datos2 <- datos2 %>%
#   filter(!is.na(GDP.billion.currency.units)) %>%
#   arrange(Year, Month)   # <- importante
# q_startEX  <- Datos2$Month[1] / 3  
# start_tsEX <- c(Datos2$Year[1], q_startEX)
# 
# # --- Series univariantes (usar start_tsEX)
# GDP_TSEX <- ts(Datos2$GDP.billion.currency.units, start = start_tsEX, frequency = 4)
# CPI_TSEX <- ts(Datos2$Consumer.Price.Index..CPI.,  start = start_tsEX, frequency = 4)
# 
# # --- Serie multivariante (usar y_multiEX + start_tsEX)
# y_multiEX <- cbind(
#   PIB = Datos2$GDP.billion.currency.units,
#   CPI = Datos2$Consumer.Price.Index..CPI.
# )
# TS_MULTIEX <- ts(y_multiEX, start = start_tsEX, frequency = 4)
# 














#Modelos Predictivos
# --- Modelos Predictivos (validación) ---
train <- window(GDP_TS, end = c(2021, 4))
test  <- window(GDP_TS, start = c(2022, 1), end = c(2022, 3))
h <- length(test)  # 3

# Modelos base + clásicos
fc_snaive <- snaive(train, h = h)
fc_rw     <- rwf(train, h = h, drift = TRUE)
fit_ets   <- ets(train);   fc_ets   <- forecast(fit_ets, h = h)
fit_arima <- auto.arima(train, seasonal = TRUE,
                        stepwise = FALSE, approximation = FALSE)
fc_arima  <- forecast(fit_arima, h = h)

# Comparación en test
acc_df <- dplyr::bind_rows(
  SNAIVE = as.data.frame(accuracy(fc_snaive, test)),
  RWDRFT = as.data.frame(accuracy(fc_rw,     test)),
  ETS    = as.data.frame(accuracy(fc_ets,    test)),
  ARIMA  = as.data.frame(accuracy(fc_arima,  test)),
  .id = "Modelo"
) %>%
  dplyr::select(Modelo, RMSE, MAE, MAPE, MASE) %>%
  arrange(RMSE)

acc_df

# --- Predicción 2022-Q3 ---
train_q3 <- window(GDP_TS, end = c(2022, 2))
fit_q3   <- ets(train_q3)
fc_q3    <- forecast(fit_q3, h = 1)
autoplot(fc_q3) + ggtitle("PIB Italia – Predicción 2022 Q3 (ETS)")
fc_q3$mean
fc_q3$lower; fc_q3$upper

# --- Predicción 2022-Q4 ---
train_q4 <- window(GDP_TS, end = c(2022, 3))
fit_q4   <- ets(train_q4)
fc_q4    <- forecast(fit_q4, h = 1)
autoplot(fc_q4) + ggtitle("PIB Italia – Predicción 2022 Q4 (ETS)")
fc_q4$mean
fc_q4$lower; fc_q4$upper



# =============================
# Modelos Predictivos (IPC)
# =============================

# --- Validación ---
ipc_train <- window(CPI_TS, end = c(2021, 4))
ipc_test  <- window(CPI_TS, start = c(2022, 1), end = c(2022, 3))
h_ipc <- length(ipc_test)  # 3

# --- Modelos base + clásicos
fc_ipc_snaive <- snaive(ipc_train, h = h_ipc)
fc_ipc_rw     <- rwf(ipc_train, h = h_ipc, drift = TRUE)
fit_ipc_ets   <- ets(ipc_train);   fc_ipc_ets   <- forecast(fit_ipc_ets, h = h_ipc)
fit_ipc_arima <- auto.arima(ipc_train, seasonal = TRUE,
                            stepwise = FALSE, approximation = FALSE)
fc_ipc_arima  <- forecast(fit_ipc_arima, h = h_ipc)

# --- Comparación en test
acc_df_ipc <- dplyr::bind_rows(
  SNAIVE = as.data.frame(accuracy(fc_ipc_snaive, ipc_test)),
  RWDRFT = as.data.frame(accuracy(fc_ipc_rw,     ipc_test)),
  ETS    = as.data.frame(accuracy(fc_ipc_ets,    ipc_test)),
  ARIMA  = as.data.frame(accuracy(fc_ipc_arima,  ipc_test)),
  .id = "Modelo"
) %>%
  dplyr::select(Modelo, RMSE, MAE, MAPE, MASE) %>%
  arrange(RMSE)

acc_df_ipc

# --- Predicción 2022-Q3 ---
ipc_train_q3 <- window(CPI_TS, end = c(2022, 2))
fit_ipc_q3   <- ets(ipc_train_q3)
fc_ipc_q3    <- forecast(fit_ipc_q3, h = 1)
autoplot(fc_ipc_q3) + ggtitle("IPC Italia – Predicción 2022 Q3 (ETS)")
fc_ipc_q3$mean
fc_ipc_q3$lower; fc_ipc_q3$upper

# --- Predicción 2022-Q4 ---
ipc_train_q4 <- window(CPI_TS, end = c(2022, 3))
fit_ipc_q4   <- ets(ipc_train_q4)
fc_ipc_q4    <- forecast(fit_ipc_q4, h = 1)
autoplot(fc_ipc_q4) + ggtitle("IPC Italia – Predicción 2022 Q4 (ETS)")
fc_ipc_q4$mean
fc_ipc_q4$lower; fc_ipc_q4$upper
