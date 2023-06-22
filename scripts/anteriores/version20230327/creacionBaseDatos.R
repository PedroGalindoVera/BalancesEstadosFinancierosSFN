# Referencias
# https://es.r4ds.hadley.nz/datos-relacionales.html

library(DBI)
library(RSQLite)
library(openxlsx)
library(readxl)

# Crea la conexión y la base de datos SQLite
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")

# Define el directorio que contiene los archivos de Excel
dir_excel <- "directorio"

# Guardamos el nombre de todas los libros
nombres_libros <- list.files(path = dir_excel, recursive = TRUE, full.names = TRUE)

# Guardamos el nombre de todas las hojas
nombres_hojas <- NULL

# Recorre todos los archivos de Excel en el directorio y sus subcarpetas
for (archivo in list.files(path = dir_excel, recursive = TRUE, full.names = TRUE)) {
  
  print(archivo)
  
  #archivo <- list.files(path = dir_excel, recursive = TRUE, full.names = TRUE)[[1]]
  
  # Abre el archivo de Excel
  wb <- openxlsx::loadWorkbook(archivo)
  
  # Guardamos el nombre de todas las tablas
  nombres_hojas <- c(nombres_hojas, sapply(archivo, function(x) readxl::excel_sheets(x)))
  
  # Recorre todas las hojas en el libro de Excel
  #for (hoja in openxlsx::getSheetNames(wb)) {
  for (hoja in sapply(archivo, function(x) readxl::excel_sheets(x))) {

    # Lee los datos de la hoja y guárdalos como un data.frame
    datos <- openxlsx::read.xlsx(wb, sheet = hoja)
    
    # Crear nombre para cada tabla
    nombre_tabla <- paste(archivo, hoja)

    # Escribe los datos de la hoja en la base de datos SQLite
    #DBI::dbWriteTable(con, name = hoja, value = datos, overwrite = TRUE)
    DBI::dbWriteTable(con, name = nombre_tabla, value = datos, overwrite = TRUE)
  }
}

# Cierra la conexión a la base de datos SQLite
DBI::dbDisconnect(con)

rm(datos)

# Eliminar base da datos
# file.remove("mydatabase.db")
