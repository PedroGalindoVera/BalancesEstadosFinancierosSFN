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
  
  descompresionZip <- function(ruta_origen, directorio_destino) {
    if ( !dir.exists(directorio_destino) )  crearDirectorio(directorio_destino)
    tryCatch(
      {
        utils::unzip(ruta_origen, exdir = directorio_destino)
      },
      error = function(e) {
        message("Ocurrió un error al descomprimir el archivo zip: ", e$message,
                "\nEmpleando 7-Zip para completar la descompresión...")
        # Código para manejar el error, utilizando una herramienta externa para descomprimir el archivo zip
        ruta_origen_normalizado <- normalizePath(ruta_origen)
        ruta_destino_normalizado <- normalizePath(directorio_destino)
        # Descompresión externa de archivos
        descompresion7zip(ruta_origen_normalizado, ruta_destino_normalizado)
      }
    )
  }
  archivoZip <- function() {
    #archivos_contenidos <- utils::unzip(ruta_origen, list = TRUE)$Name
    hay_contenidos_zip <- any(grepl("\\.zip$", archivos_contenidos))
    if ( hay_contenidos_zip ) {
      directorio_destino_temporal <-
        dirname(gsub("Descargas","Temporal",ruta_origen))
      if ( !dir.exists(directorio_destino_temporal) )  crearDirectorio(directorio_destino_temporal)
      descompresionZip(ruta_origen, directorio_destino_temporal)
      ruta_comprimido_temporal <-
        file.path(directorio_destino_temporal, archivos_contenidos)
      directorio_origen_temporal <- directorio_destino_temporal
      descomprimirArchivosDirectorio(origen = directorio_origen_temporal,
                                     destino = directorio_destino)
      unlink(ruta_comprimido_temporal, recursive = TRUE)
      #file.remove(ruta_comprimido_temporal)
    } else {
      descompresionZip(ruta_origen, directorio_destino)
    }
  }
  copiarArchivo <- function() {
    ruta_verificacion <- file.path(destino, archivo)
    if ( !file.exists(ruta_verificacion) ) {
      cat(paste0("\n[",k,"]"),"Copiando el archivo: [", normalizePath(ruta_origen),"] ...\n")
      file.copy( ruta_origen, ruta_verificacion )
    }
  }
  decidirAccion <- function() {
    tiene_extension_zip <- grepl("\\.zip$", ruta_origen)
    if ( tiene_extension_zip ) {
      archivoZip()
    } else {
      copiarArchivo()
    }
  }
  
  # Listo los archivos del directorio
  archivos_origen <- list.files(origen, recursive = TRUE)
  # Elegimos únicamente los archivos con extensión zip
  archivos <- grep("\\.zip$", archivos_origen, value = TRUE)
  # Descompresión de archivos
  k <- 0
  for ( archivo in archivos ) {
    #
    # k <- k + 1
    # archivo <- archivos[k]
    #
    ruta_origen <- file.path( origen, archivo )
    # Establecer el directorio de destino para los archivos Descomprimidos
    va_a_data_fuente_SB <-
      grepl("(?=.*data)(?=.*Fuente)(?=.*SB)", destino, perl = TRUE)
    if ( va_a_data_fuente_SB ) {
      directorio_destino <-
        dirname(gsub("Descargas|Temporal","Fuente",ruta_origen))
    } else {
      nombre_archivo_zip <- gsub("\\.zip$","",basename(ruta_origen))
      directorio_destino <- file.path(destino, nombre_archivo_zip)
    }
    if ( !dir.exists(directorio_destino) )  crearDirectorio(directorio_destino)
    archivos_contenidos <-
      utils::unzip(ruta_origen, list = TRUE)$Name
    alguno_de_los_caracteres_no_es_utf8 <-
      any(is.na(unlist(sapply(archivos_contenidos, utf8ToInt))))
    if ( !alguno_de_los_caracteres_no_es_utf8 ) {
      ruta_archivos_descomprimidos <-
        file.path(directorio_destino, archivos_contenidos)
      existe_archivo_descomprimido <-
        all(file.exists(ruta_archivos_descomprimidos))
    } else {
      existe_archivo_descomprimido <- FALSE
    }
    if ( !existe_archivo_descomprimido ) {
      decidirAccion()
      barraProgreso(archivos)
      cat("Descomprimiendo el archivo: [", normalizePath(ruta_origen), "]\n")
    }
  }
  cat("\n")
}

origen <- "data/Descargas/SB/Boletin Financiero Mensual"
destino <- "data/Fuente/SB/Boletin Financiero Mensual"
descomprimirArchivosDirectorio(origen, destino) # Verificado en prueba individual 2023/05/09


origen <- "data/Descargas/SB/Boletines Financieros Mensuales"
destino <- "data/Fuente/SB/Boletines Financieros Mensuales"
descomprimirArchivosDirectorio(origen, destino) # Verificado en prueba individual 2023/05/11


file.remove(
  grep("\\.zip$",list.files(destino, recursive = TRUE, full.names = TRUE), value = TRUE ))

