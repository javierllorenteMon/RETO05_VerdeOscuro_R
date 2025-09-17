# Librerias
library(dplyr)

# Ficheros
path_datos_originales <- 'Datos/Originales'
path_datos_limpios <- 'Datos/Limpios_1'
datos_originales <- 'titanic.csv'
datos_limpios <- 'titanic_limpios_1.csv'

datos <- read.csv(file.path(path_datos_originales,datos_originales))


# Limpieza ----
# Eliminar columnas que no sirven
datos <- datos %>% dplyr::select(-X)


# Eliminar filas con NA en la columna "Age"
# (Es por ejemplificar otro procedimiento, no es que deba hacerse esto en estos datos)
datos <- datos %>% filter(!is.na(Age))



# Escribir ----

if (!dir.exists(path_datos_limpios)) {
  dir.create(path_datos_limpios)
}

write.csv(datos, file = file.path(path_datos_limpios,datos_limpios), row.names = FALSE)



