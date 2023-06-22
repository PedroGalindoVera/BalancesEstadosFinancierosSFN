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

lista_hojas <- listaImportacionDatosFuenteSB2(nombre_hoja_buscada = "PYG")

View(lista_hojas[[1]])

#### Hoja con errores de lectura ####
tabla <- listaPYG$`BOL BANCOS PRIVADOS 31122014.xlsx PYG`
tabla <- tabla[,-1]
View(tabla)

# Requerimiento de paquetes
if (!require("stringdist")) { 
  install.packages("stringdist")
  library(stringdist)
}

tabla <- lista_hojas[[1]]

columna <- 1

filas_na <- which(is.na(tabla[,columna]))

indice_filas_na <- 1
contador <- 1


filas_na <- which(is.na(tabla[,columna]))

while ( indice_filas_na <= length(filas_na) ) {
  columna <- 1
  while ( columna <= length(tabla) ) {
    if ( is.na(tabla[filas_na[indice_filas_na], columna])  ) {
      columna <- columna + 1
    } else if ( columna == length(tabla)  ) {
      cat("\nFila vacia:", filas_na[indice_filas_na])
    } else {
      indice_filas_na <- indice_filas_na + 1
    }
    contador <- contador + 1
    cat(contador, "\r")
  }
  indice_filas_na <- indice_filas_na + 1
}


  
  
indice_columna <- which(is.na(tabla[,4]))

lista_filas_na <- list()

# Buscamos las filas que tienen valor NA en las columnas especificadas
for (columna in seq_along(tabla)) {
  lista_filas_na[[columna]] <- which(is.na(tabla[,columna]))
}

filas_na <- sort(unique(unlist(lista_filas_na)))

all(is.na(tabla[filas_na[1],]))


sapply(lista_filas_na, max)

sapply(lista_filas_na, function(elemento) elemento == 1)

which(lista_filas_na[[2]] == 1)


