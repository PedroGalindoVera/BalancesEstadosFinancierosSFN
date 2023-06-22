obtencionEnlacesDescarga <- function() {

  source("scripts/analisisVinculosPaginaWeb.R")
  
  enlaces <- analisisVinculosPaginaWeb()
  
  if (!require("httr")) {
    install.packages("httr")
  }
  
  library(httr)
  
  url <- NULL
  for (enlace in enlaces) {
    url <- c(url, httr::HEAD(enlace)$url)
    cat("\nCapturando enlaces de descarga a partir de vinculos analizados...")
  }
  
  cat("\nEnlaces de descarga reconocidos:\n")
  print(url)
  
  return(url)
}
