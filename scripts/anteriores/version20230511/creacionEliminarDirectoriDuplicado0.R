descomprimirArchivosDirectorio <- function(origen, destino) {
  
  # Esta función permite descomprimir directorios con archivos `.zip`
  
  # EJEMPLO:
  # origen <- "data/Descargas/SEPS/Bases de Datos"
  # destino <- "data/Fuente/SEPS/Bases de Datos"
  # descomprimirArchivosDirectorio(origen, destino)
  
  if (!require("utils")) {
    install.packages("utils")
    library(utils)
  }
  
  descompresion <- function(ruta_origen,ruta_destino) {
    tryCatch(
      {
        utils::unzip(ruta_origen, exdir = ruta_destino)
      },
      error = function(e) {
        message("Ocurrió un error al descomprimir el archivo zip: ", e$message,
                "\nEmpleando 7-Zip para completar la descompresión...")
        # Código para manejar el error, utilizando una herramienta externa para descomprimir el archivo zip
        ruta_origen_normalizado <- normalizePath(ruta_origen)
        ruta_destino_normalizado <- normalizePath(ruta_destino)
        # Descompresión externa de archivos
        descompresion7zip(ruta_origen_normalizado, ruta_destino_normalizado)
      }
    )
  }
  contenido_zip <- function() {
    nombre_archivos_comprimidos <- utils::unzip(ruta_origen, list = TRUE)$Name
    prueba_zip_contenidos <- any(grepl("\\.zip$", nombre_archivos_comprimidos))
    if ( prueba_zip_contenidos ) {
      ruta_destino <- destino
      descompresion(ruta_origen,ruta_destino)
      comprimido_temporal <- file.path(ruta_destino,nombre_archivos_comprimidos)
      ruta_origen_temporal <- ruta_destino
      descomprimirArchivosDirectorio(ruta_origen_temporal,ruta_destino)
      # Elimina los archivos comprimidos temporales
      unlink(comprimido_temporal, recursive = TRUE)
    } else {
      ruta_destino <- file.path( destino, gsub("\\.zip$","",basename(archivo)) )
      descompresion(ruta_origen,ruta_destino)
    }
  }
  copiado_archivo <- function() {
    ruta_verificacion <- file.path(destino, archivo)
    if ( !file.exists(ruta_verificacion) ) {
      cat(paste0("\n[",k,"]"),"Copiando el archivo: [", normalizePath(ruta_origen),"] ...\n")
      file.copy( ruta_origen, ruta_verificacion )
    }
  }
  descompresion_zip <- function() {
    prueba_extension_zip <- grepl("\\.zip$", ruta_origen)
    if ( prueba_extension_zip ) {
      contenido_zip()
    } else {
      copiado_archivo()
    }
  }
  
  # Listo los archivos del directorio
  archivos <- list.files(origen, recursive = TRUE)
  # Descompresión de archivos
  k <- 0
  for ( archivo in archivos ) {
    k <- k + 1
    ruta_origen <- file.path( origen, archivo )
    # Establecer el directorio de destino para los archivos Descomprimidos
    prueba_directorio_fuente_SB <-
      grepl("(?=.*data)(?=.*Fuente)(?=.*SB)", destino, perl = TRUE)
    if ( prueba_directorio_fuente_SB ) {
      destino <- dirname(gsub("Descargas","Fuente",ruta_origen))
    }
    if ( !dir.exists(destino) )  crearDirectorio(destino)
    descompresion_zip()
    ruta_destino_verificacion <- file.path( destino, archivo )
    if ( !file.exists(ruta_destino_verificacion) ) {
      barraProgreso(archivos)
      cat("Descomprimiendo el archivo: [", normalizePath(ruta_origen), "]\n")
    }
  }
  cat("\n")
}



directorio_principal <- "data/Fuente/SB/Boletines Financieros Mensuales"
directorios_descargas <- list.dirs(directorio_principal)

ruta_original <- "data/Fuente/SB/Boletines Financieros Mensuales/Bancos Privados/2009/2009"

k <- 173
ruta_original <- directorios_descargas[k]
carpetas_anidadas <- unlist(strsplit(ruta_original,"/"))
prueba_carpeta_duplicada <- any(duplicated(carpetas_anidadas))
if ( prueba_carpeta_duplicada ) {
  ruta_corregida <- paste(unique(carpetas_anidadas), collapse = "/")
  ruta_corregida_contenido <- paste0(ruta_corregida, "/*")
  file.rename(from = ruta_original, to = ruta_corregida_contenido)
}
