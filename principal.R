# Ingesta de datos y limpieza ----

{
  # Paso 1: Limpiar columnas sobrantes y filas con NAs
  source('Scripts_preprocesamiento/01-Limpieza.R', encoding = "UTF-8")
    
  # Paso 2: ....

}


# Limpiar entorno
rm(list=ls())


# EDA ----

{
  # Paso 1: Limpiar columnas sobrantes y filas con NAs  
  source('Scripts_EDA/11-Estadisticos.R', encoding = "UTF-8")
  
  # Paso 2: Visualizaciones EDA
  source('Scripts_EDA/12-Visualizaciones_EDA.R', encoding = "UTF-8")
  
}


# Limpiar entorno
rm(list=ls())



# MODELOS ----

{
  # Modelo 1: Regresión logística 
  source('Scripts_modelos/31-Reg_logistica.R', encoding = "UTF-8")
  
}


# Limpiar entorno
rm(list=ls())
