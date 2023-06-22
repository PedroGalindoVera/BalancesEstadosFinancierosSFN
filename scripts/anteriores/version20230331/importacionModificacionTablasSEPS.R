listaImportacionDatosFuente <- function() {
  # Determinación del directorio de las fuentes de datos descomprimidos y descargados
  directorio_principal <- "data/Fuente"
  # Listado de los archivos en el directorio señalado
  archivos <- list.files(path = directorio_principal, full.names = TRUE, recursive = TRUE)
  # Instalación condicional del paquete
  if (!require("readr")) { install.packages("readr") }
  # Llamado del paquete
  library(readr)
  # Importación de tablas en una lista
  lista_de_datos <- lapply(archivos, function (x) readr::read_delim( x, guess_max = 1000 ) )
  # lista_de_datos <- lapply(archivos, function (x) {
  #   # Probabilisticamente se elige la CODIFICACIÓN más apropiada
  #   codificacion <- readr::guess_encoding(x, n_max = 1000)$encoding[1]
  #   # Probabilisticamente se elige el DELIMITADOR más apropiado
  #   readr::read_delim( x, guess_max = 1000, locale = locale(encoding = codificacion) )
  #   readr::read_delim( x, guess_max = 1000 )
  # })
  # Resultado de la función
  return(lista_de_datos)
}

modificadorNombreColumna <- function(tabla, nombre_nuevo, ...) {
  
  ## nombre_nuevo: debe ser un string con el nombre deseado para homogenizar
  
  # Lista de palabras a buscar por columna
  palabras <- list(...)
  # Buscamos las palabras clave en los nombres de las variables
  coincidencias <- lapply(palabras, function(x) stringr::str_detect(names(tabla), stringr::regex(x, ignore_case = TRUE)))
  # Determinamos el nombre de columna a cambiar
  nombre_anterior <- names(tabla)[Reduce(`&`, coincidencias)]
  # Instalación condicional del paquete y llamado del paquete
  if (!require("rlang")) { install.packages("rlang") }
  library(rlang)
  # Se puede pasar el nuevo nombre de la columna como una variable desde fuera de la función dplyr::rename(). Para hacerlo, puedes usar la función rlang::sym() para convertir el valor de la variable en un símbolo y luego usar el operador !! para evaluarlo dentro de dplyr::rename()
  tabla <- dplyr::rename(tabla, !!rlang::sym(nombre_nuevo) := nombre_anterior)
  # Resultado de la función
  return(tabla)
}

setModificadorNombresColumnas <- function(tabla) {
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Fecha de Corte", "fecha", "corte")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Segmento", "segmento")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "RUC", "ruc")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Razón Social", "razon", "social")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Descripción", "descripcion", "cuenta")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Cuenta", "cuenta")
  tabla <- modificadorNombreColumna(tabla, nombre_nuevo = "Saldo (USD)", "saldo")
  return(tabla)
}