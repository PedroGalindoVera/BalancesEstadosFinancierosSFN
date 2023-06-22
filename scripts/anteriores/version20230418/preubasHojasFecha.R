source("scripts/herramientasImportacionModificacionTablasSB.R") # es mas rápido buscando fecha

lista_hojasBAL <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "BALANCE")
lista_hojasPYG <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "PYG")

tic <- Sys.time()
lista_fechasBAL <- sapply(lista_hojasBAL, identificarFechaCorteBoletinSB)
fechasBAL <- as.data.frame( as.Date(lista_fechasBAL, origin = "1970-01-01") )
difftime(Sys.time(),tic)

lista_fechasPYG <- sapply(lista_hojasPYG, identificarFechaCorteBoletinSB)
fechasPYG  <- as.data.frame( as.Date(lista_fechasPYG, origin = "1970-01-01") )


source("scripts/herramientasImportacionModificacionTablasSBREVISION.R")

tabla <- lista_hojasPYG[[1]]

tabla_limpia <- tabla %>% hojaToTablaBoletinesFinancierosSB() %>% View()


hojaToTablaBoletinesFinancierosSB <- function(tabla) {
  
  # Esta función aplica las transformaciones para convertir en tabla una hoja de excel de los libros con los "Boletines Financieros mensuales" de la SB.
  
  # tabla: es un data frame importado desde excel
  
  # Requerimiento de paquetes
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  # Determinamos el índice de la primera columna con valores decimales
  indice_col <- indicePrimeraColDecimalTabla(tabla)
  # Leemos la fecha del corte en cada tabla
  fecha_corte <- identificarFechaCorteBoletinSB(tabla)
  # Aplicamos todas las transformaciones
  tabla <- 
    tabla %>%
    eliminarFilasNA(tabla = ., skip = indice_col) %>%
    crearTablaBoletinMensualSB() %>%
    modificarTipoDatoTablaSB() %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(`FECHA`, everything()) #%>%
    # modificadorNombresColumnasTablasIF(tabla = .) #%>%
    # eliminarFilasCODIGOnaCUENTAna() %>%
    # eliminarFilasCODIGOnaCUENTAcod()
  
  return(tabla)
}
