directorio_principal <- "data/Fuente/SB/Boletines Financieros Mensuales"
directorios_descargas <- list.dirs(directorio_principal)

for ( k in seq_along(directorios_descargas) ) {
  ruta_original <- directorios_descargas[k]
  partes_ruta <- unlist(strsplit(ruta_original,"/"))
  indice_carpeta_anio <- grep("^[0-9]{4}$", partes_ruta)
  tiene_extension_excel <-
    grepl(".\\.xls[x]?$", partes_ruta[indice_carpeta_anio + 1]) 
  if ( !tiene_extension_excel ) {
    partes_ruta <- partes_ruta[-(indice_carpeta_anio + 1)]
  }
  tiene_carpetas_duplicadas <- any(duplicated(partes_ruta))
  if ( tiene_carpetas_duplicadas ) {
    partes_ruta <- unique(partes_ruta)
  }
  if ( !tiene_extension_excel | tiene_carpetas_duplicadas ) {
    ruta_corregida <- paste(unique(partes_ruta), collapse = "/")
  } else {
    ruta_corregida <- ruta_original
  }
  
  # Obtener una lista de todos los archivos en el directorio original
  archivos_originales <- list.files(ruta_original, full.names = TRUE)
  # Crear una lista de los nombres de archivo corregidos
  archivos_corregidos <- file.path(ruta_corregida, basename(archivos_originales))
  # Mover los archivos del directorio original al corregido
  file.copy(from = archivos_originales, to = archivos_corregidos)
  # Elimina los archivos comprimidos temporales
  unlink(ruta_original, recursive = TRUE)
  
  # tiene_carpetas_duplicadas <- any(duplicated(partes_ruta))
  # if ( tiene_carpetas_duplicadas ) {
  #   cat("[",k,"]", ruta_original,"\n")
  #   ruta_corregida <- paste(unique(partes_ruta), collapse = "/")
  #   # Obtener una lista de todos los archivos en el directorio original
  #   archivos_originales <- list.files(ruta_original, full.names = TRUE)
  #   # Crear una lista de los nombres de archivo corregidos
  #   archivos_corregidos <- file.path(ruta_corregida, basename(archivos_originales))
  #   # Mover los archivos del directorio original al corregido
  #   file.copy(from = archivos_originales, to = archivos_corregidos)
  #   # Elimina los archivos comprimidos temporales
  #   unlink(ruta_original, recursive = TRUE)
  # }
}


ruta_archivos <-
  list.files(directorio_principal, recursive = TRUE, full.names = TRUE)

ruta <- ruta_archivos[1]
partes_ruta <- unlist(strsplit(ruta_original, "/"))
indice_carpeta_anio <- grep("^[0-9]{4}$", partes_ruta)
tiene_extension_excel <-
  grepl(".\\.xls[x]?$", partes_ruta[indice_carpeta_anio + 1]) 
if ( tiene_extension_excel ) {
  nombre_carpeta <-
    paste(partes_ruta[-(indice_carpeta_anio + 1)], collapse = "/")
}

