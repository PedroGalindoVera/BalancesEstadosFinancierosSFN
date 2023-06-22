source("scripts/herramientasImportacionModificacionTablasSB.R")

listaImportacionDatosFuenteSB2 <- function(nombre_hoja_buscada = "BALANCE") {
  
  # Esta función importar como tablas las hojas con los "Balances Financieros" de los libros con los "Boletines Financieros mensuales" de la SB
  
  ## Determinación de Hojas de interés
  # Periodos de empleo del nombre para la Hoja con el "Balance Financiero" en los "Boletines Financieros mensuales"
  # "BAL": [2001,2002,2011,2012]
  # "BAL SAB II": [2008-2011]
  # "BALANCE": [2012, ...[
  
  # NOTA. Se emplea las funciones: `selectorRutasHojasLibrosDirectorio` y otras más
  
  # Se decide leer únicamente las Hojas "BALANCE" desde 2013 por que tienen una plantilla más homogénea
  # rutas_balances_SB <-
  #   selectorRutasHojasLibrosDirectorio(ruta_directorio = "data/Fuente/SB", nombre_hoja_buscar = "balance", anio_inicio = 2013)
  rutas_balances_SB <-
    selectorRutasHojasLibrosDirectorio(ruta_directorio = "data/Fuente/SB", nombre_hoja_buscar = tolower(nombre_hoja_buscada), anio_inicio = 2013)
  
  
  # Requerimiento de paquetes
  if (!require("readxl")) {
    install.packages("readxl")
    library(readxl)
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require("stringdist")) {
    install.packages("stringdist")
    library(stringdist)
  }
  
  if( exists("contador_progreso") ) rm(contador_progreso)
  
  lista_tablas <- list()
  
  for ( ruta in rutas_balances_SB){
    impresionProgreso(rutas_balances_SB)
    cat("Leyendo el archivo: [", ruta, "]\n")
    # Determinamos los nombres de las hojas de cada libro de Excel
    nombres_hojas <- readxl::excel_sheets(ruta)
    # Análisis difuso de texto para buscar la HOJA del LIBRO que coincide con el nombre buscado
    distancias <- stringdist::stringdist(tolower(nombres_hojas), tolower(nombre_hoja_buscada), method = "lv")
    # nombre_hoja_coincidencia <- nombres_hojas[distancias <= min(distancias)][1]
    nombre_hoja_coincidencia <- nombres_hojas[which.min(distancias)]
    nombre_tabla <- paste(basename(ruta),nombre_hoja_buscada)
    # Importación de la hoja buscada como data frame
    tabla <- readxl::read_excel(ruta, sheet = nombre_hoja_coincidencia)
    # Leemos la fecha del corte en cada tabla
    fecha_corte <- identificarFechaCorteBoletinSB(tabla)
    # Creamos la tabla de datos propiamente
    lista_tablas[[nombre_tabla]] <- tabla
  }
  
  return(lista_tablas)
}

lista_hojasBAL <- listaImportacionDatosFuenteSB2(nombre_hoja_buscada = "BALANCE")
lista_hojasPYG <- listaImportacionDatosFuenteSB2(nombre_hoja_buscada = "PYG")

View(lista_hojasBAL[[1]])

#### Hoja con errores de lectura ####
tabla <- lista_hojas$`BOL BANCOS PRIVADOS 31012015 act.xlsx PYG` 
indice_col <- indicePrimeraColDecimalTabla(tabla)
tabla %>%
  eliminarFilasNA(tabla = ., skip = indice_col) %>%
  View()
View(tabla)
  

tabla <- lista_hojasBAL[[1]]
tabla <- lista_hojasPYG[[102]]
indice_col <- indicePrimeraColDecimalTabla(tabla)
fecha_corte <- identificarFechaCorteBoletinSB(tabla)

tic <- Sys.time()
tabla_limpieza1 <-
  tabla %>%
    eliminarFilasNA(tabla = ., skip = indice_col) %>%
    crearTablaBoletinMensualSB() %>%
    modificarTipoDatoTablaSB() %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(`FECHA`, everything()) %>%
    modificadorNombresColumnasTablasIF(tabla = .) %>%
    eliminarFilasCODIGOnaCUENTAna()
difftime(Sys.time(), tic)

tic <- Sys.time()
tabla_limpieza2 <-
  tabla %>%
    crearTablaBoletinMensualSB() %>%
    eliminarInformacionFinTabla() %>%
    modificarTipoDatoTablaSB() %>%
    eliminarFilasSinValores() %>%
    eliminarColumnasNA() %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(`FECHA`, everything()) %>%
    modificadorNombreColumnasTablasIF(tabla = .)
difftime(Sys.time(), tic)



#### AGRUPACION CLUSTERIZACION ####
mi_vector <- unname(evalucionDecimal)
# Realizar análisis de agrupamiento jerárquico
hc <- hclust(dist(mi_vector))
# Dividir el resultado en grupos
grupos <- cutree(hc, k = 2)
grupos
primera_columna_decimal <- which(grupos>1)[1]


tabla <- lista_hojasPYG[[102]]
tabla <- lista_hojas[[80]]

indice_col <- indicePrimeraColDecimalTabla(tabla)

tabla_limpia <- tabla %>%
  crearTablaBoletinMensualSB() %>%
  eliminarFilasNA(tabla = ., skip = indice_col) %>%
  View()

