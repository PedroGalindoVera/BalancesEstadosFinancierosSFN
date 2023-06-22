importarLibroExcel <- function(ruta_libro, hoja_libro, fila_inicio = 0, fila_fin) {
  extension_libro <- tools::file_ext(ruta_libro)
  if ( extension_libro %in% c("xls","xlsx") ) {
    hoja <- readxl::read_excel(ruta_libro, sheet = hoja_libro, skip = 0, n_max = fila_fin)
  } else if ( extension_libro == "xlsm" ) {
    hoja <- xlsx::read.xlsx(ruta_libro, sheetName = hoja_libro, startRow = 0, endRow = fila_fin)
  } else if ( extension_libro == "xlsb" ) {
    #hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, range = "A1:Z20", skip = fila_inicio)
    hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, skip = fila_inicio, skip = fila_inicio)
  } else {
    cat("\nNo se soporta el formato del archivo.")
  }
}

importarLibroExcel <- function(ruta_libro, hoja_libro, fila_inicio = NULL, fila_fin = NULL) {
  extension_libro <- tools::file_ext(ruta_libro)
  fila_inicio <- ifelse(is.null(fila_inicio), 0, fila_inicio)
  if ( extension_libro %in% c("xls","xlsx","xlsm") ) {
    fila_fin <- ifelse(is.null(fila_fin), Inf, fila_fin)
    hoja <- readxl::read_excel(ruta_libro, sheet = hoja_libro, skip = fila_inicio, n_max = fila_fin)
  } else if ( extension_libro == "xlsb" ) {
    #hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, range = "A1:Z20", skip = fila_inicio)
    hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, skip = fila_inicio)
  } else {
    cat("\nNo se soporta el formato del archivo.")
  }
}

importarLibroExcel <- function(ruta_libro, hoja_libro, fila_inicio = 0, fila_fin = Inf) {
  extension_libro <- tools::file_ext(ruta_libro)
  if ( extension_libro %in% c("xls","xlsx","xlsm") ) {
    hoja <- readxl::read_excel(ruta_libro, sheet = hoja_libro, skip = fila_inicio, n_max = fila_fin)
  } else if ( extension_libro == "xlsb" ) {
    #hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, range = "A1:Z20", skip = fila_inicio)
    hoja <- readxlsb::read_xlsb(ruta_libro, sheet = hoja_libro, skip = fila_inicio)
  } else {
    cat("\nLa función soporta únicamente los formatos: xls, xlsx, xlsm, xlsb")
  }
}

hoja <- importarLibroExcel(ruta_libro, hoja_libro) 


ruta_libro <- "data/Fuente/SB/PUBLICA/2020/BOL_FIN_PUB_SEPT_20.xlsb"
ruta_libro <- rutas_libros_seleccionados[1]
hoja_libro <- "BALANCE"
fila_inicio <- 0
fila_fin <- 30
hoja <- importarLibroExcel(ruta_libro, hoja_libro, fila_fin) 

df <- data.frame(ruta = rutas_libros_seleccionados)
df$extension <- tools::file_ext(rutas_libros_seleccionados)

# QUIZA EL FORMATO ES EL QUE ALTERA LA LECTURA DEL NOMBRE COLUMNAS EN EL SKIP
unique(tools::file_ext(list.files("data/Fuente/SB/PUBLICA", recursive = TRUE)))
unique(tools::file_ext(rutas_libros_seleccionados))
hoja <- importarLibroExcel(ruta_libro, hoja_libro)
indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
tabla <- importarLibroExcel(ruta_libro, hoja_libro, fila_inicio = indice_fila_nombres_columnas)
