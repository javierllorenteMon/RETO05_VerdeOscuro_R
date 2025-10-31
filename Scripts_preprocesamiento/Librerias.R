#Instalar librerias en caso de que no esten todas
packages <- c(
  "tidyverse",    
  "janitor",
  "zoo",
  "readxl",
  "plotly",
  "skimr",
  "corrplot",
  "psych",
  "lubridate",
  "gridExtra",
  "htmlwidgets",
  "webshot2",
  "openxlsx",
  "naniar",
  "forecast",
  "fpp2",
  "tseries",
  "cowplot",
  "car"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    message("Instalando paquete faltante: ", pkg)
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
}