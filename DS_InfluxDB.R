#remotes::install_github("influxdata/influxdb-client-r")
library(influxdbclient)
library(httr)
library(readr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readxl)

# Read.csv
data<- read.csv(file = "/home/unai/Reto05/exogenas_paises_punto2.csv")
summary(data)

client<-InfluxDBClient$new(url="http://localhost:8086",
                           token="1U7I1lG2Q_EXocIL7DqsEI_JqzuNRYk0P5halpqAYm0U7Spd5lsHen4WaoF_mGJ-7Q7ljyYfRnbtc9LXLl2Qdw==",
                           org="mondragon")

client$health()
check<- client$ready()

INFLUX_TOKEN="1U7I1lG2Q_EXocIL7DqsEI_JqzuNRYk0P5halpqAYm0U7Spd5lsHen4WaoF_mGJ-7Q7ljyYfRnbtc9LXLl2Qdw=="
Influx_Authorization_Header <- paste ("Token", INFLUX_TOKEN)
INFLUX_URL="http://localhost:8086/api/v2/buckets"
INFLUX_ORG_ID='{
"name": "Data_Science_Reto05",
"orgID": "eff228e9b0ba705e"
}'


response <- POST(INFLUX_URL,
                 body = INFLUX_ORG_ID,
                 accept("application/csv"),
                 content_type("application/json"),
                 add_headers(Authorization = Influx_Authorization_Header)
)

#Data formating
data_limpio<- data[data$Country == "Italy", ]
data_limpio<-  data_limpio %>%
  mutate(
    time = ymd(paste(Year, Month, "01", sep="-"))
  )

data_limpio$Stock.market.index<-as.numeric(data_limpio$Stock.market.index)
data_limpio$Unemployment.rate.percent<-as.numeric(data_limpio$Unemployment.rate.percent)
data_limpio$time<-as.POSIXct(data_limpio$time, tz="UTC")

data_limpio<- data_limpio %>%
  filter(!is.na(Stock.market.index), !is.na(Unemployment.rate.percent))

colnames(data_limpio)
str(data_limpio)

#Write data
response <- client$write(data_limpio, bucket = "Data_Science_Reto05", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code"),
                         fieldCols = c("Stock.market.index","Unemployment.rate.percent"),
                         timeCol = "time")

#Query
# 1. Consulta básica - Obtener todos los datos de Italia
query1 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 1996-01-01T00:00:00Z)
  |> filter(fn: (r) => r["_measurement"] == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")'

query1<- gsub("[\r \n]", "", query1)
result1 <- client$query(query1)
print("Consulta 1 - Todos los datos de Italia:")
print(result1)

# 2. Consulta del índice bursátil de Italia en los últimos 5 años
query2 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 2018-01-01T00:00:00Z)
  |> filter(fn: (r) => r["_measurement"]  == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")
  |> filter(fn: (r) => r["_field"] == "Stock.market.index")'

query2<- gsub("[\r \n]", "", query2)
result2 <- client$query(query2)
print("Consulta 2 - Índice bursátil de Italia últimos 5 años:")
print(result2)

# 3. Consulta de tasa de desempleo promedio por año
query3 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 1996-01-01T00:00:00Z)
  |> filter(fn: (r) => r["_measurement"]  == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")
  |> filter(fn: (r) => r["_field"] == "Unemployment.rate.percent")
  |> aggregateWindow(every: 1y, fn: mean)'

query3<- gsub("[\r \n]", "", query3)
result3 <- client$query(query3)
print("Consulta 3 - Tasa de desempleo promedio anual:")
print(result3)

# 4. Consulta comparativa - Ambos campos en un período específico (2008 crisis)
query4 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 2007-01-01T00:00:00Z, stop: 2009-12-31T23:59:59Z)
  |> filter(fn: (r) => r["_measurement"]  == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")
  |> filter(fn: (r) => r["_field"] == "Stock.market.index" or r["_field"] == "Unemployment.rate.percent")'

query4 <- gsub("\n", " ", query4)
result4 <- client$query(query4)
print("Consulta 4 - Datos durante la crisis 2007-2009:")
print(result4)

# 5. Consulta con agregaciones - Estadísticas del índice bursátil
query5 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 1996-01-01T00:00:00Z)
  |> filter(fn: (r) => r["_measurement"] == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")
  |> filter(fn: (r) => r["_field"] == "Stock.market.index")
  |> aggregateWindow(every: 1y, fn: max)
  |> yield(name: "max_anual")'

query5<- gsub("[\r \n]", "", query5)
result5 <- client$query(query5)
print("Consulta 5 - Máximos anuales del índice bursátil:")
print(result5)

# 6. Consulta de tendencia - Datos mensuales del último año completo
query6 <- 'from(bucket: "Data_Science_Reto05")
  |> range(start: 2021-01-01T00:00:00Z, stop: 2021-12-31T23:59:59Z)
  |> filter(fn: (r) => r["_measurement"] == "Italy")
  |> filter(fn: (r) => r["Code"] == "ITA")
  |> filter(fn: (r) => r["_field"] == "Stock.market.index" or r["_field"] == "Unemployment.rate.percent")
  |> aggregateWindow(every: 1mo, fn: mean)'

query6 <- gsub("\n", " ", query6)
result6 <- client$query(query6)
print("Consulta 6 - Datos mensuales promedio del año 2021:")
print(result6)