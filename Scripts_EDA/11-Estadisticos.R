# Librerias
library(readxl) 
library(dplyr)     
library(ggplot2)   
library(skimr)    
library(corrplot)  

# 1. Leer ficheros
pib_ipc<- read.csv('Datos/Originales/pib_ipc_paises_punto2.csv')
exogenas<- read.xlsx('Datos/Originales/exogenas_paises_punto2.xlsx')

exogenas[6:8] <- lapply(exogenas[6:8], function(x) as.numeric(gsub("[^0-9.-]", "", x)))

pib_ipc <- pib_ipc %>%
  filter(!is.na(GDP.billion.currency.units)) %>%
  arrange(Year, Month)


# 2. Exploración inicial
glimpse(pib_ipc)
glimpse(exogenas)

summary(pib_ipc)
summary(exogenas)

skim(pib_ipc)
skim(exogenas)

# 3. Ver valores faltantes
colSums(is.na(pib_ipc))
colSums(is.na(exogenas))

# 4. Estadísticas descriptivas por país
Est_pib<- pib_ipc %>% 
  group_by(Country) %>% 
  summarise(across(c(GDP.billion.currency.units, Consumer.Price.Index..CPI.), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

Est_exogenas<- exogenas %>% 
  group_by(Country) %>% 
  summarise(across(c(Money.supply.billion.currency.units, Unemployment.rate.percent, Stock.market.index), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

# 5. Graficar tendencias de PIB e IPC por país
ggplot(pib_ipc, aes(x = Year, y = GDP.billion.currency.units, color = Country)) +
  geom_line(size=1) +
  theme_minimal() +
  labs(title="Evolución del PIB por país", y="PIB", x="Año")

ggplot(pib_ipc, aes(x = Year, y = Consumer.Price.Index..CPI., color = Country)) +
  geom_line(size=1) +
  theme_minimal() +
  labs(title="Evolución del IPC por país", y="IPC", x="Año")

# 6. Si hay variables exógenas, comparar con PIB o IPC
merged_data <- pib_ipc %>%
  inner_join(exogenas, by = c("Country", "Year"))

# Matriz de correlación
num_vars <- merged_data %>% select(where(is.numeric))
corr_matrix <- cor(num_vars, use = "pairwise.complete.obs")

corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black")

# 7. Graficar relación PIB con alguna exógena (ej. inflación, tasa de interés)
ggplot(merged_data, aes(x = Stock.market.index, y = GDP.billion.currency.units, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title="Relación entre índice del mercado de valores y PIB")
