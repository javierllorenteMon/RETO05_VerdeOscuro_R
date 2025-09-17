library(caTools)


# Leer fichero 
path_datos <- 'Datos/Limpios_1'
datos_limpios <- 'titanic_limpios_1.csv'

datos <- read.csv(file.path(path_datos,datos_limpios))


# Train-test split
split <- sample.split(datos, SplitRatio = 0.8)

data_train <- subset(datos, split == "TRUE")
data_test <- subset(datos, split == "FALSE")

# Training model
logistic_model <- glm(Survived ~ Age, 
                      data = data_train, 
                      family = "binomial")



# Crear carpeta para guardar resultados del modelo ----

path <- 'Resultados_modelos'
if (!dir.exists(path)) {
  dir.create(path)
}


# Usar sink() para que escriba en un fichero el output de consola
sink(file.path(path,'modelo_1_reg_logistica_survived_vs_age.txt'))
logistic_model
sink()

# Guardar modelo en .rda
save(logistic_model, file = file.path(path,'modelo_1_reg_logistica_survived_vs_age.rda'))




