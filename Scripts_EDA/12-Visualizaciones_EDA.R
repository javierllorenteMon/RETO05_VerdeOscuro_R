# Librerias
library(dplyr)
library(ggplot2)

# Leer fichero 
path_datos <- 'Datos/Limpios_1'
datos_limpios <- 'titanic_limpios_1.csv'

datos <- read.csv(file.path(path_datos,datos_limpios))

# Crear carpeta para guardar plots ----

path_graficos <- 'Graficos'
if (!dir.exists(path_graficos)) {
  dir.create(path_graficos)
}

path_graficos_EDA <- 'Graficos/EDA'
if (!dir.exists(path_graficos_EDA)) {
  dir.create(path_graficos_EDA)
}



# Boxplots ----

p <- datos %>% 
  ggplot(aes(x=as.factor(Sex))) +
  geom_bar(fill = "dodgerblue") +
  labs(x="Sexo", y = "Número de pasajeros") + 
  theme_minimal()

print(p)

ggsave(filename = file.path(path_graficos_EDA,'boxplot_sex.png'), plot = p)


# Histogramas ----

p <- datos %>% 
  ggplot(aes(x=Age)) +
  geom_histogram(fill = "dodgerblue", binwidth = 5) +
  labs(x="Edad", y = "Número de pasajeros") + 
  theme_minimal()

print(p)

ggsave(filename = file.path(path_graficos_EDA,'histograma_age.png'), plot = p)

