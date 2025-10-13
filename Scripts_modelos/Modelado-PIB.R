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
train_PIB <- window(PIB_sinO, start = c(2000, 1), end = c(2021, 1))
test_PIB <- window(PIB_sinO, start = c(2021, 2), end = c(2022, 3))

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

# ------ ARIMA (auto.arima con seasonal = FALSE) ------
modelo_PIB_Arima <- auto.arima(train_PIB_est, seasonal = FALSE)
summary(modelo_PIB_Arima)
checkresiduals(modelo_PIB_Arima)

pred_PIB_Arima <- forecast(modelo_PIB_Arima, h = length(test_PIB))

# Recuperar el último valor conocido del log(PIB)
ultimo_valor <- as.numeric(log(tail(train_PIB, 2))) # 2 porque se han aplicado 2 diferencias

# Revertir la diferencia (cumsum = acumulado)
pred_PIB_revert_A <- exp(cumsum(as.numeric(pred_PIB_Arima$mean)) + ultimo_valor)

# Comparar con el test real
accuracy_PIB_Arima <- accuracy(pred_PIB_revert_A, test_PIB)
accuracy_PIB_Arima

# ------ SARIMA (auto.arima con seasonal = TRUE) ------
modelo_PIB_sarima <- auto.arima(train_PIB_est, seasonal = TRUE)
summary(modelo_PIB_sarima)
checkresiduals(modelo_PIB_sarima)

pred_PIB_sarima <- forecast(modelo_PIB_sarima, h = length(test_PIB))

# Revertir la diferencia (cumsum = acumulado)
pred_PIB_revert_S <- exp(cumsum(as.numeric(pred_PIB_sarima$mean)) + ultimo_valor)

# Comparar con el test real
accuracy_PIB_sarima <- accuracy(pred_PIB_revert_S, test_PIB)
accuracy_PIB_sarima

# ------ Modelos Naive y SNaive ------
pred_naive <- naive(train_PIB, h = length(test_PIB))
pred_snaive <- snaive(train_PIB, h = length(test_PIB))

accuracy_naive <- accuracy(pred_naive$mean, test_PIB)
accuracy_snaive <- accuracy(pred_snaive$mean, test_PIB)

# Graficar e interpretar resultados (solo Arima y Sarima)

# Crear data frames para graficar
df_test <- data.frame(Trimestre = time(test_PIB), PIB = as.numeric(test_PIB))
df_pred_arima <- data.frame(Trimestre = time(test_PIB), Pred = as.numeric(pred_PIB_revert_A))
df_pred_sarima <- data.frame(Trimestre = time(test_PIB), Pred = as.numeric(pred_PIB_revert_S))

# Graficar ARIMA
plot_arima <- ggplot() +
  geom_line(data = df_test, aes(x = Trimestre, y = PIB), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_arima, aes(x = Trimestre, y = Pred), color = 'red', linetype = 'dashed', size = 1.1) +
  labs(title = 'Pronóstico ARIMA vs PIB real', y = 'PIB', x = 'Trimestre') +
  theme_minimal()

# Graficar SARIMA
plot_sarima <- ggplot() +
  geom_line(data = df_test, aes(x = Trimestre, y = PIB), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_sarima, aes(x = Trimestre, y = Pred), color = 'green', linetype = 'dashed', size = 1.1) +
  labs(title = 'Pronóstico SARIMA vs PIB real', y = 'PIB', x = 'Trimestre') +
  theme_minimal()

# Mostrar ambos gráficos juntos
grid.arrange(plot_arima, plot_sarima, ncol = 1)

# Con todos los modelos

# Crear data frames para graficar
Trimestres <- time(test_PIB)
df_test <- data.frame(Trimestre = Trimestres, PIB = as.numeric(test_PIB))
df_pred_arima <- data.frame(Trimestre = Trimestres, Pred = as.numeric(pred_PIB_revert_A))
df_pred_sarima <- data.frame(Trimestre = Trimestres, Pred = as.numeric(pred_PIB_revert_S))
df_naive <- data.frame(Trimestre = Trimestres, Pred = as.numeric(pred_naive$mean))
df_snaive <- data.frame(Trimestre = Trimestres, Pred = as.numeric(pred_snaive$mean))


# Graficar todos los modelos
plot_modelos <- ggplot() +
  geom_line(data = df_test, aes(x = Trimestre, y = PIB), color = 'blue', size = 1.1) +
  geom_line(data = df_pred_arima, aes(x = Trimestre, y = Pred), color = 'red', linetype = 'dashed', size = 1.1) +
  geom_line(data = df_pred_sarima, aes(x = Trimestre, y = Pred), color = 'green', linetype = 'dashed', size = 1.1) +
  geom_line(data = df_naive, aes(x = Trimestre, y = Pred), color = 'purple', linetype = 'dotted', size = 1.1) +
  geom_line(data = df_snaive, aes(x = Trimestre, y = Pred), color = 'orange', linetype = 'dotted', size = 1.1) +
  labs(title = 'Pronóstico PIB: ARIMA, SARIMA, Naive y SNaive vs Real', y = 'PIB', x = 'Trimestre') +
  theme_minimal()


plot_modelos

