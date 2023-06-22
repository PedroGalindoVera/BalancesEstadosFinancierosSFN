descargarBoletinesFinancierosSB <- function(ruta_archivo_html) {
  
  requerirPaquetes("rvest","dplyr","stringr","readr")
  
  leer_pagina_html <- function(ruta_archivo_html, codificacion = NA) {
    if ( is.na(codificacion) ) {
      codificacion <-
        readr::guess_encoding(ruta_archivo_html) %>%
        slice(which.max(confidence)) %>%
        pull(encoding)
    }
    pagina_html <- rvest::read_html(ruta_archivo_html, encoding = codificacion)
    return(pagina_html)
  }
  scraping_descarga_entry_file <- function(pagina_html, informacion_enlaces_data_frame) {
    nodos_descarga_entry_file <- pagina_html %>% html_nodes(".entry.file")
    if ( length(nodos_descarga_entry_file) > 0 ) {
      informacion_enlaces <- data.frame(
        boletin =
          pagina_html %>% html_nodes(".entry.file") %>% html_attr("data-name"),
        nombre_archivo =
          pagina_html %>% html_nodes(".entry.file") %>%
          html_nodes(".entry-info-name span") %>% html_text(),
        enlace_descarga =
          pagina_html %>% html_nodes(".entry_link.entry_action_download") %>% #no se puede separar el selector
          html_attr("href"),
        ids_archivo =
          pagina_html %>% html_nodes(".entry.file") %>% html_attr("data-id"),
        fecha_modificacion =
          pagina_html %>% html_nodes(".entry-info-modified-date") %>% html_text(),
        fecha_descripcion =
          pagina_html %>% html_node(".description-file-info") %>% html_text() %>%
          stringr::str_extract("\\d+\\s[a-zA-Z]+,\\s\\d{4}\\s\\d+:\\d+\\s[ap]m"),
        tamano_archivo =
          pagina_html %>% html_nodes(".entry-info-size") %>% html_text() %>%
          readr::parse_number()
      )
    } else {
      informacion_enlaces <- informacion_enlaces_data_frame
    }
    return(informacion_enlaces)
  }
  scraping_descarga_a <- function(pagina_html, informacion_enlaces_data_frame) {
    nodos_descarga_entry_file <- pagina_html %>% html_nodes(".entry.file")
    nodos_descarga_a <- pagina_html %>% html_nodes("a")
    if ( length(nodos_descarga_entry_file) == 0 &
         length(nodos_descarga_a) > 0
    ) {
      informacion_enlaces <-
        data.frame(enlaces_descarga = nodos_descarga_a %>% html_attr("href")) %>%
        mutate(nombre_archivo = basename(enlaces_descarga))
    } else {
      informacion_enlaces <- informacion_enlaces_data_frame
    }
    return(informacion_enlaces)
  }
  scraping_descarga_error <- function(informacion_enlaces_data_frame) {
    if ( nrow(informacion_enlaces_data_frame) == 0 ) {
      stop(paste("\nEl proceso se ha interrumpido,",
                 "el Web Scraping no pudo realizarse,",
                 "la función 'descargarBoletinesFinancierosSB'",
                 "requiere mantenimiento."))
    }
  }
  directorioCarpetaDescarga <- function(ruta_archivo_html, nombre_archivo) {
    directorio_carpeta <-
      ruta_archivo_html %>%
      gsub(".html", "",.) %>%
      gsub("html/", "data/Descargas/",.)
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
    crearDirectorio(directorio_carpeta)
    return(directorio_carpeta)
  }
  
  pagina_html <- leer_pagina_html(ruta_archivo_html)
  informacion_enlases_selecionados <- data.frame()
  informacion_enlases_selecionados <- scraping_descarga_entry_file(pagina_html, informacion_enlases_selecionados)
  informacion_enlases_selecionados <- scraping_descarga_a(pagina_html, informacion_enlases_selecionados)
  scraping_descarga_error(informacion_enlases_selecionados)
  
  exportarReporteTabla(
    dataFrame =  informacion_enlases_selecionados,
    nombre_archivo = paste("Reporte Enlaces de Descarga SB"))
  
  #barraProgresoReinicio()
  
  for (k in 1:nrow(informacion_enlases_selecionados) ) {
    link <- informacion_enlases_selecionados$enlace_descarga[k]
    nombre_archivo <- informacion_enlases_selecionados$nombre_archivo[k]
    directorio_descarga <-
      directorioCarpetaDescarga(ruta_archivo_html, nombre_archivo)
    ruta_archivo <- file.path(directorio_descarga, nombre_archivo)
    #tamanio_archivo <- informacion_enlases_selecionados$tamanos_archivo[k]
    if ( 
      !file.exists(ruta_archivo) #| file.size(ruta_archivo) < tamanio_archivo
    ) {
      download.file(link, ruta_archivo, mode = "wb", timeout = 300)
      cat("\033[1;32mSe descargó el archivo en la ruta:\033[0m",
          "[", normalizePath(ruta_archivo),"]\n\n")
      #barraProgreso(seq_along(download_links_elegidos))
      #cat("Descargando... ")
    }
  }
}
