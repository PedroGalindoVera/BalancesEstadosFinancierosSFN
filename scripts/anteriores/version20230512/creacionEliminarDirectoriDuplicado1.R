eliminarArchivosZipEnDirectorio <- function(ruta_directorio) {
  # EJEMPLO:
  # ruta_directorio <- "data/Fuente/SB/Boletines Financieros Mensuales"
  # eliminarArchivosZipEnDirectorio(ruta_directorio)
  rutas_archivos <-
    list.files(ruta_directorio, recursive = TRUE, full.names = TRUE)
  rutas_zip_eliminar <- grep("\\.zip$", rutas_archivos, value = TRUE)
  unlink(rutas_zip_eliminar, recursive = TRUE)
}

depurarDirectorioAnio <- function(directorio_principal) {
  
  # EJEMPLO:
  # directorio_principal <- "data/Fuente/SB/Boletines Financieros Mensuales"
  # depurarDirectorioAnio(directorio_principal)
  
  rutas_archivos <-
    list.files(directorio_principal, recursive = TRUE, full.names = TRUE)
  for ( ruta_archivo in rutas_archivos ) {
    
    # tiene_extension_zip <- grepl("\\.zip$", basename(ruta_archivo))
    # if ( tiene_extension_zip ) unlink(ruta_archivo, recursive = TRUE)
    
    partes_ruta <- unlist(strsplit(ruta_archivo, "/"))
    directorio_archivo <- paste(partes_ruta[1:6], collapse = "/")
    nombre_archivo <- tail(partes_ruta, 1)
    ruta_corregida <- file.path(directorio_archivo, nombre_archivo)
    
    son_rutas_diferentes <- ruta_corregida != ruta_archivo
    if ( son_rutas_diferentes & !tiene_extension_zip ) {
      file.copy(from = ruta_archivo, to = directorio_archivo)
      unlink(ruta_archivo, recursive = TRUE)
    }
    directorio_original <- dirname(ruta_archivo)
    
    es_directorio_vacio <-
      length(list.files(directorio_original, recursive = TRUE)) == 0
    if ( es_directorio_vacio ) unlink(directorio_original, recursive = TRUE)
    
    # tamanio_directorio_eliminar <- file.info(directorio_original)$size
    # es_directorio_vacio <- tamanio_directorio_eliminar == 0
    # if ( es_directorio_vacio ) unlink(directorio_original, recursive = TRUE)
  }
}

directorio_principal <- "data/Fuente/SB/Boletines Financieros Mensuales"
depurarDirectorioAnio(directorio_principal)
k <- 198

eliminarDirectoriosVacios <- function(directorio_principal) {
  directorios <-
    list.dirs(directorio_principal, recursive = TRUE, full.names = TRUE)
  numero_subcarpetas <- sapply(strsplit(directorios,"/"), length)
  directorios_eliminar <- directorios[numero_subcarpetas > 6]
  tamanio_directorio_eliminar <- file.info(directorios_eliminar)$size
  es_directorio_vacio <- tamanio_directorio_eliminar == 0
  directorios_eliminar <- directorios_eliminar[es_directorio_vacio]
  unlink(directorios_eliminar, recursive = TRUE)
}

eliminarDirectoriosVacios(directorio_principal)








