# Librerias
library(dplyr)

# Leer fichero 
path_datos <- 'Datos/Limpios_1'
datos_limpios <- 'titanic_limpios_1.csv'

datos <- read.csv(file.path(path_datos,datos_limpios))


# Summaries ----
print("SUMMARY")
summary(datos) %>% print()

print("------------")

print("VALORES ÚNICOS DE SEX")
datos %>% pull(Sex) %>% unique() %>% print()

print("------------")



