
# Test adf : sirve para detectar si queda tendencia estocástica.
test_adf <- function(serie){ 
  resultado <- adf.test(serie) 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test ADF ===\n") 
    cat("p-valor:", pvalor, "→ Serie ESTACIONARIA\n\n") } 
  else { cat("=== Test ADF ===\n") 
    cat("p-valor:", pvalor, "→ Serie NO estacionaria\n\n") 
  } 
}

# Test kpss : lo contrario a adf, dice si queda tendencia determinista o varianza no constante.
test_kpss <- function(serie){ 
  resultado <- kpss.test(serie, null="Level") 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test KPSS ===\n") 
    cat("p-valor:", pvalor, "→ Serie NO estacionaria\n\n") 
  } 
  else { 
    cat("=== Test KPSS ===\n") 
    cat("p-valor:", pvalor, "→ Serie ESTACIONARIA\n\n") 
  } 
}

# Test LB : dice si los residuos de un modelo se comportan como ruido blanco, se usa despues de ajustar un modelo.
test_lb <- function(serie, lags=12){ 
  resultado <- Box.test(serie, lag=lags, type="Ljung-Box") 
  pvalor <- resultado$p.value 
  if(pvalor < 0.05){ 
    cat("=== Test Ljung-Box ===\n") 
    cat("p-valor:", pvalor, "→ Serie con autocorrelación (NO ruido blanco)\n\n") } 
  else { 
    cat("=== Test Ljung-Box ===\n") 
    cat("p-valor:", pvalor, "→ Serie SIN autocorrelación (ruido blanco)\n\n") 
  }
}

# Juntar todos los test en una sola funcion
test_estacionariedad <- function(serie, nombre="Serie") {
  cat("=== ", nombre, " ===\n")
  test_adf(serie)
  test_kpss(serie)
  test_lb(serie)
  cat("\n---------------------------\n")
}