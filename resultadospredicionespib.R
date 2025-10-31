library(dplyr)
library(zoo)
library(ggplot2)

PIB_vals <- window(PIB_sinO, start=c(2021,2), end=c(2022,2))
fechas_hist <- as.yearqtr(time(PIB_vals))

PIB_hist_df <- data.frame(
  Fecha = fechas_hist,
  PIB_Real = as.numeric(PIB_vals)
)

PIB_real_Q3_Q4 <- data.frame(
  Fecha = as.yearqtr(c("2022 Q3", "2022 Q4")),
  PIB_Real = c(502.027, 510.632)
)

PIB_total_real <- bind_rows(PIB_hist_df, PIB_real_Q3_Q4) %>%
  arrange(Fecha)

Pred_PIB_Q3_Q4 <- data.frame(
  Fecha = as.yearqtr(c("2022 Q3", "2022 Q4")),
  PIB_Prediccion = c(475.8786, 510.6689)
)

PIB_comparacion <- PIB_total_real %>%
  left_join(Pred_PIB_Q3_Q4, by = "Fecha") %>%
  mutate(
    Pct_Real = round(100 * (PIB_Real - lag(PIB_Real)) / lag(PIB_Real), 2),
    
    Pct_Prediccion = case_when(
      Fecha == as.yearqtr("2022 Q3") ~ round(100 * (PIB_Prediccion - lag(PIB_Real)) / lag(PIB_Real), 2),
      Fecha == as.yearqtr("2022 Q4") ~ round(100 * (PIB_Prediccion - lag(PIB_Prediccion)) / lag(PIB_Prediccion), 2),
      TRUE ~ NA_real_
    )
  )

print(PIB_comparacion)

