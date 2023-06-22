descargarBoletinesFinancierosSB <- function(ruta_archivo_html) {
  library(rvest)
  library(dplyr)
  
  #
  directorioCarpetaDescarga <- function(ruta_archivo_html, nombre_archivo) {
    directorio_carpeta <- gsub(".html", "", ruta_archivo_html)
    directorio_carpeta <- gsub("html/", "data/Descargas/", directorio_carpeta)
    anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
    expresion_regular_anios <- paste(seq(1990,anio_actual), collapse = "|")
    prueba_anio <- grepl(expresion_regular_anios, directorio_carpeta)
    if ( !prueba_anio ) {
      nombre_carpeta <- gsub(".zip", "", nombre_archivo)
      coincidencias_anio <- gregexpr(expresion_regular_anios, nombre_carpeta)
      nombre_carpeta <- unlist(regmatches(nombre_carpeta, coincidencias_anio))
      directorio_carpeta <- 
        gsub(basename(directorio_carpeta), nombre_carpeta, directorio_carpeta)
    }
    if ( !dir.exists(directorio_carpeta) ) crearDirectorio(directorio_carpeta)
    return(directorio_carpeta)
  }
  #
  
  codigo_html <- rvest::read_html(ruta_archivo_html)
  download_links <-
    codigo_html %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  nombre_archivo <-
    codigo_html %>% rvest::html_nodes("span") %>% rvest::html_text()
  # Asumimos biyección entre el "id" del enlace y la fecha del Boletín Mensual
  unique_ids <- unique(sapply(download_links, function(link) {
    sub(".*id=([a-zA-Z0-9]+)&.*", "\\1", link)
  }))
  download_links_id <-
    sapply(unique_ids, function(id) grep(id, download_links, value = TRUE))
  
  prueba_matrix <- any(class(download_links_id) %in% c("matrix","array"))
  if ( prueba_matrix ) {
    # [1,] tarda demasiado en empezar a descargar
    # [3,] parece descargar más rápidamente
    download_links_elegidos <- download_links_id[3,]
    names(download_links_elegidos) <- nombre_archivo
  }
  
  # anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
  # expresion_regular_anios <- paste(seq(1990,anio_actual), collapse = "|")
  #prueba_anio <- grepl(expresion_regular_anios, directorio_carpeta)
  
  prueba_char <- any(class(download_links_id) %in% "character")
  if ( prueba_char ) {
    download_links_elegidos <- download_links_id
    names(download_links_elegidos) <- basename(download_links_id)
  }
  
  if (exists("contador_progreso")) rm(contador_progreso, envir = .GlobalEnv)
  
  for (k in seq_along(download_links_elegidos) ) {
    link <- download_links_elegidos[k]
    nombre_archivo <- names(download_links_elegidos)[k]
    directorio_descarga <-
      directorioCarpetaDescarga(ruta_archivo_html, nombre_archivo)
    ruta_archivo <- file.path(directorio_descarga, nombre_archivo)
    if ( !file.exists(ruta_archivo) | file.size(ruta_archivo) == 0 ) {
      download.file(link, ruta_archivo, mode = "wb", timeout = 300)
      cat("Se descargó el archivo en la ruta: [", normalizePath(ruta_archivo),"]\n\n")
      barraProgreso(seq_along(download_links_elegidos))
      cat("Descargando... ")
    }
  }
}

ejecutarDescargaBoletinesFinancierosSB <- function() {
  ruta_directorio_html_SB <- "html/SB/Boletines Financieros Mensuales"
  rutas_archivos_html_SB <-
    list.files(ruta_directorio_html_SB, recursive = TRUE, full.names = TRUE)
  for (ruta in rutas_archivos_html_SB) {
    descargarBoletinesFinancierosSB(ruta)
    barraProgreso(rutas_archivos_html_SB)
    cat("Analizando archivo html en la ruta: [", ruta, "]\n\n")
  }
}

k <- 2

ruta_archivo_html <- rutas_archivos_html_SB[k]
descargarBoletinesFinancierosSB(ruta_archivo_html)

download_links_elegidos[1]
head <- httr::HEAD(download_links_elegidos[1])



carpeta <- gsub(".zip", "", basename(download_links_id))
grep("([19][09][0-9][0-9])", carpeta, value = TRUE)

anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
expresion_regular_anios <- paste(seq(1990,anio_actual), collapse = "|") 
unlist(regmatches(carpeta, gregexpr(expresion_regular_anios, carpeta)))


regmatches(carpeta, gregexpr("([129][09][0-9][0-9])", carpeta))