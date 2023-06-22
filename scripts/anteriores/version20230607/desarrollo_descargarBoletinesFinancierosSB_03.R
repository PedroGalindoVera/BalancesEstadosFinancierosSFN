descargarBoletinesFinancierosSB <- function(ruta_archivo_html) {
  
  requerirPaquetes("rvest","dplyr","stringr","purrr")
  
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
    #if ( !dir.exists(directorio_carpeta) )
    crearDirectorio(directorio_carpeta)
    return(directorio_carpeta)
  }
  
  pagina <- rvest::read_html(ruta_archivo_html)
  
  informacion_archivos <-
    pagina %>% 
    rvest::html_nodes(".entry.file") %>% 
    purrr::map_df(~{
      data.frame(
        ids_archivo = .x %>% html_attr("data-id"),
        nombres_archivo = .x %>% html_node(".entry-info-name span") %>% html_text(),
        dias = .x %>% html_node(".entry-info-modified-date") %>% html_text(),
        fechas = .x %>% html_node(".description-file-info") %>% html_text() %>%
          str_extract("\\d+\\s[a-zA-Z]+,\\s\\d{4}\\s\\d+:\\d+\\s[ap]m"),
        tamanos_archivo = .x %>% html_node(".entry-info-size") %>% html_text() %>%
          parse_number()
      )
    })
  
  informacion_enlaces <-
    data.frame(
      enlaces_descarga =
        pagina %>% html_nodes(".entry.file .entry_action_download") %>% html_attr("href")
    ) %>%
    mutate(
      ids_enlaces = stringr::str_extract(enlaces_descarga, "(?<=id=)[^&]+")
    ) %>%
    inner_join(informacion_archivos, by = c("ids_enlaces" = "ids_archivo"))
  
  informacion_enlases_selecionados <-
    informacion_enlaces %>%
    distinct(ids_enlaces, .keep_all = TRUE)
  
  exportarReporteTabla(
    dataFrame =  informacion_enlases_selecionados,
    nombre_archivo = paste("Reporte Enlaces de Descarga SB"))
  
  #barraProgresoReinicio()
  
  for (k in 1:nrow(informacion_enlases_selecionados) ) {
    link <- informacion_enlases_selecionados$enlaces_descarga[k]
    nombre_archivo <- informacion_enlases_selecionados$nombres_archivo[k]
    directorio_descarga <-
      directorioCarpetaDescarga(ruta_archivo_html, nombre_archivo)
    ruta_archivo <- file.path(directorio_descarga, nombre_archivo)
    tamanio_archivo <- informacion_enlases_selecionados$tamanos_archivo[k]
    if ( 
      !file.exists(ruta_archivo) |
      file.size(ruta_archivo) < tamanio_archivo ) {
      download.file(link, ruta_archivo, mode = "wb", timeout = 300)
      cat("\033[1;32mSe descargó el archivo en la ruta:\033[0m",
          "[", normalizePath(ruta_archivo),"]\n\n")
      #barraProgreso(seq_along(download_links_elegidos))
      #cat("Descargando... ")
    }
  }
}
