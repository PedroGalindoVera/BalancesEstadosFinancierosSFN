cracionDirectorio <- function(directorio_base = getwd(), nueva_ruta) {
  # Ejemplo: cracionDirectorio(nueba_ruta = "data")
  # Ejemplo: cracionDirectorio(directorio_base = "data", nueva_ruta = "data/Descargas")
  lista_ruta_subcarpetas <- list.dirs(directorio_base, recursive = FALSE)
  if ( ! (basename(nueva_ruta) %in% basename(lista_ruta_subcarpetas) ) ) {
    dir.create(nueva_ruta)
    cat("\nSe creo la carpeta: [",basename(nueva_ruta),"] en el directorio: [", directorio_base,"].\n")
  } else {
    cat("\nLa carpeta: [", basename(nueva_ruta), "] ya existe en el directorio: [", directorio_base,"].\n")
  }
}

exportarReporteTabla <- function(dataFrame, nombre_archivo) {
  # Install and load the openxlsx package
  if (!require("openxlsx")) { install.packages("openxlsx") }
  library(openxlsx)
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  # Add a worksheet to the workbook
  openxlsx::addWorksheet(wb, "Reporte")
  # Write the data frame to the worksheet and automatically adjust column widths
  openxlsx::writeData(wb, "Reporte", dataFrame) #, autoWidth = TRUE)
  # Directorio
  rep_dir <- "data/Reportes"
  cracionDirectorio(directorio_base = "data", rep_dir)
  # Nombre archivo
  nombre_archivo <- paste(nombre_archivo, format(Sys.time(), "%Y-%m-%d_%HH%M.xlsx"))
  # Save the workbook to an Excel file
  openxlsx::saveWorkbook(wb, file.path(rep_dir, nombre_archivo), overwrite = TRUE)
  #openxlsx::write.xlsx(informacion, file.path(rep_dir,paste("Reporte Enlaces de Descarga",format(Sys.Date(), "%Y-%m-%d.xlsx"))), rowNames = FALSE)
}

analisisVinculosPaginaWebSEPS <- function() {
  if (!require("rvest")) { install.packages("rvest") }
  library(rvest)
  
  link <- "https://estadisticas.seps.gob.ec/index.php/estadisticas-sfps/"
  pagina <- rvest::read_html(link)
  
  div_BaseDatos <-
    rvest::html_nodes(pagina, xpath = '//*[@id="collapse_4" or @id="collapse_5"]')
  
  links_BaseDatos <-
    rvest::html_nodes(div_BaseDatos,"a") %>%
    rvest::html_attr("href")
  
  cat("\nVínculos rescatados de la página analizada:\n")
  print(links_BaseDatos)
  
  return(links_BaseDatos)
}

analisisVinculosPaginaWebSB <- function() {
  if (!require("rvest")) { install.packages("rvest") }
  library(rvest)
  
  link <- "https://www.superbancos.gob.ec/estadisticas/portalestudios/bancos/"
  pagina <- rvest::read_html(link)
  
  # Seleccionar los enlaces de descarga
  enlaces_descarga <- html_nodes(pagina, "a[href*=download]")
  
  # Extraer las URLs de descarga
  urls_descarga <- html_attr(enlaces_descarga, "href")
  
  cat("\nVínculos rescatados de la página analizada:\n")
  print(urls_descarga)
  
  return(urls_descarga)
}

analisisVinvulosDirectorioWebSB <- function() {
  if (!require("rvest")) { install.packages("rvest") }
  library(rvest)
  
  link <- "https://www.superbancos.gob.ec/estadisticas/wp-content/uploads/sites/4/downloads/"
  
  # Lee el contenido de la página
  pagina <- rvest::read_html(link)
  
  # Selecciona todos los elementos "ancla" que contienen los enlaces de descarga
  enlaces <- pagina %>% rvest::html_nodes("a")
  
  # Extrae las URLs de los enlaces de descarga
  urls_descarga <- enlaces %>% rvest::html_attr("href")
  
  # Crea las rutas completas de los archivos y carpetas
  # `file.path` aumenta un "/" entre las dos rutas, por lo que se usa paste0
  rutas_completas <- paste0(link, urls_descarga) 
  
  # Filtra solo las URLs que terminan en ".zip"
  urls_zip <- urls_descarga[grepl("\\.zip$", urls_descarga)]
  
  # Crea las rutas completas de los archivos zip
  rutas_completas_zip <- paste0(link, urls_zip)
  
  cat("\nVínculos rescatados del repositorio web analizada:\n")
  #print(rutas_completas_zip)
  
  return(rutas_completas_zip)
}

obtencionEnlacesDescarga <- function(enlaces_descarga, identificador) {
  
  enlaces <- enlaces_descarga
  
  if (!require("httr")) { install.packages("httr") }
  library(httr)
  
  url <- NULL
  status_code <- NULL
  content_type <- NULL
  content_length <- NULL
  last_modified <- NULL
  k <- 0
  for (enlace in enlaces) {
    head <- httr::HEAD(enlace)
    url <- c(url, head$url)
    status_code <- c(status_code, head$status_code)
    content_type <- c(content_type, head$headers$`content-type`)
    content_length <- c(content_length, ifelse(!is.null(head$headers$`content-length`), head$headers$`content-length`, NA))
    last_modified <- c(last_modified, ifelse(!is.null(head$headers$`last-modified`), head$headers$`last-modified`, NA))
    k <- k + 1
    if ( k == 1 ) { cat("\nRutas de descarga:") }  
    cat(paste0("\n[",k,"]"),"Del vínculo:\n\t[", enlace, "]\nse ha capturado la ruta de descarga:\n\t [", head$url, "].\n")
  }
  
  informacion <-
    data.frame(
      link = enlaces,
      url = url,
      status_code = status_code,
      content_type = content_type,
      last_modified = last_modified,
      content_length = round(as.numeric(content_length) / 2^20, 2)
    )
  
  cat("\n\nResumen:\n")
  print(informacion)
  
  exportarReporteTabla(dataFrame =  informacion, nombre_archivo = paste("Reporte Enlaces de Descarga", identificador))
  
  return(informacion)
}



descargaArchivosEnlacesAnalizados <- function(enlaces, informacion, identificador) {
  
  desc_dir <- file.path("data/Descargas",identificador)
  
  cracionDirectorio(nueva_ruta = "data")
  
  cracionDirectorio(directorio_base = "data", nueva_ruta = "data/Descargas")
  
  cracionDirectorio(directorio_base = "data", nueva_ruta = desc_dir)
  
  url <- informacion$url
  
  status <- informacion$status_code
  
  for ( k in 1:length(url) ) {
    dest_file <- file.path(desc_dir, basename(url[k]))
    if ( !file.exists(dest_file) && status[k] == 200) {
      # El argumento `timeout = 120` indica que R esperará hasta 120 segundos antes de cancelar la descarga si no recibe una respuesta del servidor.
      download.file(url[k], dest_file, timeout = 120)
    } else if ( file.exists(dest_file) ) {
      cat(paste0("\n[",k,"]"), "El archivo: [", basename(url[k]), "] ya existe en el directorio: [", dest_file, "].\n")
    } else if ( status[k] == 404 ) {
      cat("\nEl archivo: [", basename(url[k]), "], NO está disponible en la dirección: [", url[k], "].\n")
    }
  }
}

descompresionArchivosSubcomprimidos <- function(ruta) {
  
  # Argumentos ejemplo:
  # desc_dir <- "data/Descargas/SEPS"
  
  desc_dir <- ruta
  
  # Listamos los archivos en el directorio de  Descargas
  archivos_comprimidos <- file.path(desc_dir, list.files(desc_dir))
  
  # Consulta meta datos de los archivos comprimidos para buscar subcomprimidos
  meta_archivos_comprimidos <- sapply(archivos_comprimidos, function(x) unzip(x, list = TRUE))
  
  # Archivos subcomprimidos
  archivos_subcomprimidos <- meta_archivos_comprimidos["Name",]
  
  # Verificamos si hay archivos subcomprimidos y los descomprimimos en Descargas
  if (any(grepl("zip", archivos_subcomprimidos))) {
    
    indices_subcomprimidos <- which(grepl("zip", archivos_subcomprimidos))
    
    directorios_comprimidos <- colnames(meta_archivos_comprimidos)[indices_subcomprimidos]
    
    if (!require("tools")) { install.packages("tools") }
    library(tools)
    
    for ( dir_comp in directorios_comprimidos) {
      if ( tools::file_ext(dir_comp) == "zip" ) {
        unzip(dir_comp, exdir = desc_dir)
        file.remove(dir_comp)
      }
    }
  }
}

descompresionArchivos <- function(origen, destino) {
  
  # Argumentos ejemplo:
  # origen = "data/Descargas/SEPS"
  # destino = "data/Fuente/SEPS"
  
  # Establecer el directorio de destino para los archivos Descromprimidos
  cracionDirectorio(directorio_base = "data", nueva_ruta = "data/Fuente")
  
  # Establecer el directorio de destino para los archivos Descromprimidos
  cracionDirectorio(directorio_base = "data/Fuente", nueva_ruta = destino)
  
  # Listo los archivos del directorio
  archivos <- list.files(origen)
  
  if (!require("tools")) { install.packages("tools") }
  library(tools)
  
  # Descompresión de archivos
  for ( archivo in archivos ) {
    file <- file.path( origen, basename(archivo) ) # version util en SEPS
    dest_nombre_carpeta <- file.path( destino, tools::file_path_sans_ext(archivo) )
    dir.create(dest_nombre_carpeta)
    if ( file_ext(archivo) == "zip" ) {
      cat("\nDescomprimiendo el archivo:", archivo,"...\n")
      unzip(file, exdir = dest_nombre_carpeta)
    }
  }
}

descompresionDirectorio <- function(origen, destino) {
  
  # Argumentos ejemplo:
  # origen = "data/Descargas"
  # destino = "data/Fuente"
  
  # Listar directorios en ruta_directorio especificada
  carpetas <- list.dirs(origen, recursive = FALSE)
  
  for (carpeta in carpetas) {
    destino = file.path(destino, basename(carpetas))
    descompresionArchivos(origen = carpeta, destino = destino)
  }
}

descompresionArchivosSB <- function(origen, destino, num_archivo) {
  
  # Argumentos ejemplo:
  # origen = "data/Descargas/SEPS"
  # destino = "data/Fuente/SEPS"
  # num_archivo = 100 # Fuerza a la función a seguir descomprimiendo en un punto especifico
  
  # Establecer el directorio de destino para los archivos Descromprimidos
  cracionDirectorio(directorio_base = "data", nueva_ruta = "data/Fuente")
  
  # Establecer el directorio de destino para los archivos Descromprimidos
  cracionDirectorio(directorio_base = "data/Fuente", nueva_ruta = destino)
  
  # Listo los archivos del directorio
  archivos <- list.files(origen, recursive = TRUE)
  
  if (!require("tools")) { install.packages("tools") }
  library(tools)
  
  # Descompresión de archivos
  k <- 0
  for ( archivo in archivos[num_archivo:length(archivos)] ) {
    file <- file.path( origen, archivo ) # version util en SEPS
    dest_nombre_carpeta <- file.path( destino, dirname(archivo) )
    dir.create(dest_nombre_carpeta)
    if ( file_ext(archivo) == "zip" ) {
      cat(paste0("\n[",k,"]"),"Descomprimiendo el archivo:", archivo,"...\n")
      k <- k + 1
      unzip(file, exdir = dest_nombre_carpeta)
    }
  }
}
