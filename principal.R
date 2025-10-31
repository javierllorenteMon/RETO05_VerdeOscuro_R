###############################################################
# RETO 05 — VERDE OSCURO — ITALIA
###############################################################

# Limpiar entorno inicial
rm(list = ls())

###############################################################
# 1 PREPROCESAMIENTO DE DATOS
###############################################################

# 1-1 Instalar Librerias utilizadas para este proyecto
source("Scripts_preprocesamiento/Librerias.R", encoding = "UTF-8")
rm(list = ls())

# 1-2 Limpieza de datos y preparación de datos para modelado
source("Scripts_preprocesamiento/LimpiezaDatos.R", encoding = "UTF-8")
rm(list = ls())

# 1-3 Limpieza de datos Istat
source("Scripts_preprocesamiento/LimpiezaIstat.R", encoding = "UTF-8")
rm(list = ls())


###############################################################
# 2 MODELADO
###############################################################

# 2-1 Modelado PIB y graficos
source("Scripts_modelos/Modelado_PIB.R", encoding = "UTF-8")
rm(list = ls())

# 2-2 Modelado IPC y graficos
source("Scripts_modelos/Modelado_IPC.R", encoding = "UTF-8")
rm(list = ls())

###############################################################
# 3 ANÁLISIS EXPLORATORIO Y RESULTADOS
###############################################################

# 3-1 Estadísticos descriptivos
source("Scripts_EDA/Estadisticos_1.R", encoding = "UTF-8")
source("Scripts_EDA/Estadisticos_2.R", encoding = "UTF-8")
rm(list = ls())

# 3-2 Comparaciones de predicciones IPC
source("Scripts_EDA/Comparacion_Pred_IPC.R", encoding = "UTF-8")
rm(list = ls())

# 3-3 Comparaciones de predicciones PIB
source("Scripts_EDA/Comparacion_Pred_PIB.R", encoding = "UTF-8")
rm(list = ls())

