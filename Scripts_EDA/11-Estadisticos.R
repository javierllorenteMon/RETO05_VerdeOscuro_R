# Librerias
library(readxl) 
library(dplyr)     
library(ggplot2)   
library(skimr)    
library(corrplot)  
library(tidyr)
library(psych)
library(lubridate)

# 1. Leer ficheros
pib_ipc<- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
exogenas<- read_xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

pib_ipc[, 4:5] <- lapply(pib_ipc[, 4:5], as.character)
exogenas[, 4:5] <- lapply(pib_ipc[, 4:5], as.character)
exogenas[, 6:8] <- lapply(exogenas[, 6:8], function(x) as.numeric(gsub("[^0-9.-]", "", x)))

pib_ipc <- pib_ipc %>%
  filter(!is.na(GDP.billion.currency.units)) %>%
  arrange(Year, Month)


# 2. Exploración inicial
glimpse(pib_ipc)
glimpse(exogenas)

summary(pib_ipc)
summary(exogenas)

# 3. Filtrar solo Italia
italia_pib_ipc <- pib_ipc %>% filter(Country == "Italy")
italia_exogenas <- exogenas %>% filter(Country == "Italy")

# 4. Análisis descriptivo PIB e IPC
summary(italia_pib_ipc)

# 5. Ver valores faltantes
colSums(is.na(pib_ipc))
colSums(is.na(exogenas))

# Evolución temporal del PIB
# Crear columna de fecha (usando el primer día de cada mes)
italia_pib_ipc <- italia_pib_ipc %>%
  mutate(
    Año_trimestre = paste(Year, sprintf("%02d", as.numeric(Month)))
  ) %>%
  arrange(Año_trimestre)



ggplot(italia_pib_ipc, aes(x = Año_trimestre, y = GDP.billion.currency.units, group=1)) +
  geom_line(color="steelblue", size=1.2) +
  geom_point() +
  labs(title="Evolución del PIB en Italia", y="PIB", x="Fecha (Trimestre)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Evolución temporal del IPC
ggplot(italia_pib_ipc, aes(x = Año_trimestre, y = Consumer.Price.Index..CPI., group=1)) +
  geom_line(color="darkred", size=1.2) +
  geom_point() +
  labs(title="Evolución del IPC en Italia", y="IPC", x="Año") +
  theme_minimal()

# --- 5. Unir con variables exógenas ---
italia_exogenas <- italia_exogenas %>%
  mutate(
    Año_trimestre = paste(Year, sprintf("%02d", as.numeric(Month)))
  ) %>%
  arrange(Año_trimestre)

italia <- italia_pib_ipc %>%
  left_join(italia_exogenas, by = c("Año_trimestre"))

# --- 6. Matriz de correlación ---
num_vars <- italia %>% select_if(is.numeric)
cor_matrix <- cor(num_vars, use="complete.obs")

corrplot(cor_matrix, method="color", type="upper",
         tl.col="black", tl.cex=0.8, title="Correlaciones en Italia")

# --- 7. Boxplots para detectar variabilidad ---
italia %>%
  pivot_longer(cols = where(is.numeric),
               names_to = "Variable",
               values_to = "Valor") %>%
  ggplot(aes(y=Valor, fill=Variable)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title="Distribución de variables en Italia") +
  theme(legend.position="none")

###########################################
# Descriptivos de las variables numéricas en Italia
describe(italia %>% select(where(is.numeric)))

# Series temporales (evolución en el tiempo)
italia_long <- italia %>%
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Valor")

ggplot(italia_long, aes(x=Año_trimestre, y=Valor, color=Variable)) +
  geom_line(size=1) +
  facet_wrap(~Variable, scales="free_y") +
  theme_minimal() +
  labs(title="Evolución temporal de las variables en Italia")

#Correlaciones más visuales
num_vars <- italia %>% select(where(is.numeric))
cor_matrix <- cor(num_vars, use="complete.obs")

corrplot(cor_matrix, method="number", type="upper",
         tl.col="black", tl.cex=0.8, number.cex=0.7)

#Histogramas de distribución
italia_long %>%
  ggplot(aes(x=Valor, fill=Variable)) +
  geom_histogram(bins=20, alpha=0.7) +
  facet_wrap(~Variable, scales="free") +
  theme_minimal() +
  labs(title="Distribución de las variables en Italia")