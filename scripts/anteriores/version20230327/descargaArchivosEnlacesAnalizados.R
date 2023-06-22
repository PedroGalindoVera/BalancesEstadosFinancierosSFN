descargaArchivosEnlacesAnalizados <- function() {

  # Llamado al codigo que obtiene los enlaces de descarga
  source("scripts/obtencionEnlacesDescarga.R")
  
  # Obtención de los enlaces de descarga
  url <- obtencionEnlacesDescarga()
  
  # Listado de carpetas del directorio data
  subcarpetas <- list.dirs("data", recursive = FALSE)
  nombres_subcarpetas <- basename(subcarpetas)
  
  # Directorio para las descargas
  desc_dir <- "data/Descargas"
  
  # Creamos el directorio para las descargas
  if ( !(basename(desc_dir) %in% nombres_subcarpetas) ) {
    dir.create(desc_dir)
  }
  
  # Descarga robusta de todos los enlaces verificando los existentes
  for ( k in 1:length(url) ) {
    dest_file <- file.path(desc_dir, basename(url[[k]]))
    if ( !file.exists(dest_file) ) {
      download.file(url[[k]], dest_file)
    } else {
      print("El archivo ya existe.")
    }
  }

}
