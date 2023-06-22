# General----

crearDirectorio <- function(nueva_ruta) {
  
  # Esta función permite crear cualesquier ruta especificada, dentro del directorio del proyecto.
  
  # EJEMPLO: crearDirectorio("data/Fuente/SB/PRIVADA")
  
  if ( !dir.exists(nueva_ruta)  ) {
    # Separamos los nombres de las carpetas de la nueva ruta especificada
    nombre_carpetas <- unlist(strsplit(nueva_ruta, "/"))
    # Recursivamente creamos los subdirectoiros especificados
    for ( k in seq_along(nombre_carpetas) ) {
      # Definimos la ruta concatenada
      subdirectorio <- paste(head(nombre_carpetas, k), collapse = "/")
      if (!dir.exists(subdirectorio)) {
        # Creamos el subdirectorio especificado
        dir.create(subdirectorio)
        cat("\nSe creo la carpeta: [",basename(subdirectorio),"] con la ruta: [", normalizePath(subdirectorio),"].\n")
      } 
    }
  } else {
    cat("\nLa carpeta: [", basename(nueva_ruta), "] ya existe en el directorio con ruta: [", normalizePath(nueva_ruta),"].\n")
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
  #creacionDirectorio(directorio_base = "data", rep_dir)
  crearDirectorio(rep_dir)
  # Nombre archivo
  nombre_archivo <- paste0(nombre_archivo, format(Sys.time(), " %Y-%m-%d_%HH%M.xlsx"))
  # Ruta archivo
  ruta_archivo <- file.path(rep_dir, nombre_archivo)
  # Save the workbook to an Excel file
  openxlsx::saveWorkbook(wb, ruta_archivo, overwrite = TRUE)
  #openxlsx::write.xlsx(informacion, file.path(rep_dir,paste("Reporte Enlaces de Descarga",format(Sys.Date(), "%Y-%m-%d.xlsx"))), rowNames = FALSE)
  cat("\nSe ha creado el archivo con la ruta: [", normalizePath(ruta_archivo), "]\n")
}

actualizarReporte <- function(dataFrame, nombre_archivo) {
  
  directorio <- "data/Reportes"
  # Define la ruta del archivo
  file_path <- file.path(directorio,paste0(nombre_archivo,".xlsx"))
  
  # Verifica si el archivo ya existe
  if (file.exists(file_path)) {
    # Si el archivo existe, lee su contenido
    existing_data <- openxlsx::read.xlsx(file_path)
    # Agrega las nuevas filas al final
    new_data <- rbind(existing_data, dataFrame)
  } else {
    # Si el archivo no existe, usa el data frame proporcionado
    new_data <- dataFrame
  }
  
  # Guarda el nuevo data frame en el archivo de Excel
  write.xlsx(new_data, file_path)
  
  cat("\nSe ha actualizado el archivo con la ruta: [", normalizePath(file_path), "]\n")
}

barraProgreso <- function(conjunto) {
  barra_progreso <- txtProgressBar(min = 0, max = length(conjunto), style = 3)
  nelementos <- length(conjunto)
  if ( exists("contador_progreso") ) {
    setTxtProgressBar(barra_progreso, contador_progreso)
    marcador_cronometro_progreso <- Sys.time()
    tiempo_transcurrido <- difftime(marcador_cronometro_progreso,inicio_cronometro_progreso,units = "sec")
    estimador_tiempo_proceso <- nelementos*(tiempo_transcurrido/(contador_progreso))
    cat("\nTiempo transcurrido:",
        format(as.POSIXct(as.numeric(tiempo_transcurrido), origin = "1970-01-01", tz = "UTC"),"%H:%M:%S"),
        " de ", format(as.POSIXct(as.numeric(estimador_tiempo_proceso), origin = "1970-01-01", tz = "UTC"),"%H:%M:%S")
    )
    contador_progreso <<- contador_progreso + 1
  } else {
    inicio_cronometro_progreso <<- Sys.time()
    contador_progreso <<- 1
  }
  cat(paste0("\n[",contador_progreso,"] "))
  if ( contador_progreso == length(conjunto) ) {
    close(barra_progreso)
    rm(contador_progreso, envir = .GlobalEnv)
    rm(inicio_cronometro_progreso, envir = .GlobalEnv)
  }
}

analisisCaracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
  
  # Permite identificar en un vector de texto los caracteres ajenos a la
  # escritura en español, y retornar una tabla de los reconocimientos más
  # plausibles.
  
  vectorTexto2palabras <- function(vector_texto, separadores, expresion_regular) {
    texto_unico <- unique(vector_texto)
    texto_separado <- unlist(strsplit(texto_unico, separadores))
    palabras <- sort(unique(texto_separado))
    #incide_ocurrencia <- grepl(expresion_regular,palabras)
    #return(palabras[incide_ocurrencia])
    vector_palabras <- grep(expresion_regular, palabras, value = TRUE)
    return(vector_palabras)
  }
  palabrasCorrectas <- function(vector_texto) {
    ##expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]+$"
    ##expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9\\(\\)-]+$"
    #expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9\\(\\)\"-]+$"
    #separadores <- "[ .;/-]"
    expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]+$"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  palabrasIncorrectas <- function(vector_texto, patron_incorrecto = NULL) {
    expresion_regular <-
      if ( is.null(patron_incorrecto) ) {
        ##"[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)-]"
        #"[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]" # "–" es diferente de "-"
        "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]"
      } else {
        patron_incorrecto
      }
    #separadores <- "[ .;/-]"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  caracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
    expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+"
    vector_texto_unico <- sort(unique(vector_texto))
    if ( length(vector_texto_unico) > 100000  ) {
      if ( is.null(certidumbre) ) certidumbre = 0.05
      tamanio_total <- length(vector_texto_unico)
      tamanio_muestra <-
        max(min(10000, tamanio_total), ceiling(certidumbre*tamanio_total))
      vector_muestra <- sample(vector_texto_unico, tamanio_muestra)
      vector_proceso <- vector_muestra
    } else {
      vector_proceso <- vector_texto_unico
    }
    ocurrencias <- gregexpr(expresion_regular, vector_proceso)
    extraccion <- regmatches(vector_proceso, ocurrencias)
    hallazgos <- sort(unique(unlist(extraccion)))
    hallazgos <- gsub("\\?","\\\\?",hallazgos)
    return(hallazgos)
  }
  caracterDiscrepante <- function(palabra_incorrecta, palabra_correcta) {
    
    # Identifica hasta un carácter discrepaste entre dos cadenas de texto
    # EJEMPLO: caracterDiscrepante("BAÃ‘OS", "BAÑOS")
    
    cadena_incorrecta <- as.character(palabra_incorrecta)
    cadena_correcta <- as.character(palabra_correcta)
    caracteres_cadena1 <- unlist(strsplit(cadena_incorrecta, ""))
    caracteres_cadena2 <- unlist(strsplit(cadena_correcta, ""))
    caracteres_comunes <- intersect(caracteres_cadena1,caracteres_cadena2)
    # caracteres_conjuntos <- union(caracteres_cadena1,caracteres_cadena2)
    # caracteres_diferentes <- setdiff(caracteres_conjuntos,caracteres_comunes) #
    # tail(caracteres_diferentes,1)
    caracteres_diferentes1 <- setdiff(caracteres_cadena1,caracteres_cadena2)
    caracteres_diferentes2 <- setdiff(caracteres_cadena2,caracteres_cadena1)
    frecuencia_caracteres_cadena1 <- table(caracteres_cadena1)
    frecuencia_caracteres_cadena2 <- table(caracteres_cadena2)
    caracter_incorrecto <- as.character()
    caracter_correcto <- as.character()
    discrepancias_frecuencia_caracteres <- array()
    prueba_unicidad <-
      (all(frecuencia_caracteres_cadena1 == 1) &
         all(frecuencia_caracteres_cadena2 == 1)) |
      all(frecuencia_caracteres_cadena1[caracteres_comunes] == 
            frecuencia_caracteres_cadena2[caracteres_comunes])
    if ( prueba_unicidad ) {
      caracter_incorrecto <- paste(caracteres_diferentes1, collapse = "")
      caracter_correcto <- paste(caracteres_diferentes2, collapse = "")
    }
    if ( length(caracteres_comunes) > 0 ) {
      discrepancias_frecuencia_caracteres <-
        (frecuencia_caracteres_cadena1[caracteres_comunes] !=
           frecuencia_caracteres_cadena2[caracteres_comunes])
    }
    if ( length(caracteres_diferentes1) > 0 & length(caracteres_diferentes2) == 0 ) {
      caracter_incorrecto <- paste(caracteres_diferentes1, collapse = "")
    }
    if ( length(caracteres_diferentes1) == 0 & length(caracteres_diferentes2) > 0 ) {
      caracter_incorrecto <- paste(caracteres_diferentes2, collapse = "")
    }
    if ( exists("discrepancias_frecuencia_caracteres") ) {
      if ( any(discrepancias_frecuencia_caracteres) ) {
        caracter_correcto <- caracteres_comunes[discrepancias_frecuencia_caracteres]
      }
    }
    
    return(list(incorrecto = caracter_incorrecto, correcto = caracter_correcto))
    
  }
  textoConCaracteresCorrectos <- function(vector_texto) {
    texto_unico <- sort(unique(vector_texto))
    expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+$"
    texto_correcto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_correcto)
  }
  textoConCaracteresIncorrectos <- function(vector_texto) {
    texto_unico <- sort(unique(vector_texto))
    expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]"
    texto_incorrecto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_incorrecto)
  }
  
  cat("\n\nAnalizando caracteres extraños...\n")
  if (exists("contador_progreso")) rm(contador_progreso, envir = .GlobalEnv)
  
  fraces_unicas <- sort(unique(vector_texto))
  palabras_correctas <- palabrasCorrectas(fraces_unicas)
  caracteres_incorrectos <- caracteresIncorrectos(fraces_unicas, certidumbre)
  caracter <- data.frame()
  lista_palabras <- list()
  lista_distancias <- list()
  for (k in seq_along(caracteres_incorrectos)) {
    barraProgreso(seq_along(caracteres_incorrectos))
    caracter_incorrecto <- caracteres_incorrectos[k]
    palabras_incorrectas <-
      palabrasIncorrectas(fraces_unicas, caracter_incorrecto)
    distancia <-
      stringdist::stringsimmatrix(
        palabras_correctas, palabras_incorrectas, method = "jw")
    rownames(distancia) <- palabras_correctas
    colnames(distancia) <- palabras_incorrectas
    if ( any(distancia > 0) ) {
      incidice_palabra_maximal <- apply(distancia, 2, which.max)
      palabra <- data.frame(
        original = palabras_incorrectas,
        similar = palabras_correctas[incidice_palabra_maximal])
      caracteres_identificados <-
        sapply(seq_len(nrow(palabra)),
               function(k) caracterDiscrepante(
                 palabra$original[k], palabra$similar[k])$correcto)
      tabla_frecuencias <- table(caracteres_identificados)
      caracter_identificado <- names(which.max(tabla_frecuencias))
    } else {
      frace_incorrecta <- grep(caracter_incorrecto, fraces_unicas, value = TRUE)
      frace_correcta <- textoConCaracteresCorrectos(fraces_unicas)
      distancia <- stringdist::stringsimmatrix(frace_correcta, frace_incorrecta)
      rownames(distancia) <- frace_correcta
      colnames(distancia) <- frace_incorrecta
      indice_frace_maximal <- which.max(distancia)
      frace_identificada <- frace_correcta[indice_frace_maximal]
      caracter_identificado <-
        setdiff(
          unlist(strsplit(frace_identificada, " ")),
          unlist(strsplit(frace_incorrecta, " ")))
      palabra <-
        data.frame(
          original = frace_incorrecta,
          similar = frace_identificada)
    }
    caracter[k, c("original","identificado")] <-
      c(caracter_incorrecto, caracter_identificado)
    nombre_abributo <-
      paste0("\"",caracter_incorrecto,"\" ~ \"",caracter_identificado,"\"")
    lista_palabras[[nombre_abributo]] <- palabra
    lista_distancias[[nombre_abributo]] <- distancia
  }
  return(
    list(caracter = data.frame(stats::na.omit(caracter), row.names = NULL),
         lista_palabras = lista_palabras,
         lista_distancias = lista_distancias))
}

correcionCaracteresParalelizada <- function(vector_texto) {
  
  cat("\n\nCorrigiendo caracteres extraños...\n")
  if (exists("contador_progreso")) rm(contador_progreso, envir = .GlobalEnv)
  
  analisis_caracteres <- analisisCaracteresIncorrectos(vector_texto)$caracter
  cat("\n\nLista de caracteres a corregir:")
  analisis_caracteres
  caracter_incorrecto <- c("  ", analisis_caracteres$original)
  caracter_correcto <- c(" ", analisis_caracteres$identificado)
  correcciones <-
    stats::setNames(caracter_correcto, caracter_incorrecto)
  bloques_texto <-
    split(vector_texto, seq_along(vector_texto) %% parallel::detectCores())
  texto_corregido_parelizado <-
    parallel::mclapply(
      bloques_texto,
      function(bloque) {
        barraProgreso(bloques_texto)
        cat("Correción de caracteres paralelizada ...")
        stringr::str_replace_all(bloque, correcciones)
      })
  texto_corregido <- unlist(texto_corregido_parelizado)
  return(texto_corregido)
}

# Descarga----

analisisVinculosPaginaWebSEPS <- function() {
  
  # Paquete para analisis de texto
  if (!require("rvest")) { 
    install.packages("rvest")
    library(rvest)
  }
  
  # Paquete para analisis de texto
  if (!require("dplyr")) { 
    install.packages("dplyr")
    library(dplyr)
  }
  
  link <- "https://estadisticas.seps.gob.ec/index.php/estadisticas-sfps/"
  pagina <- rvest::read_html(link)
  
  # En el código original de la página se han identificado los nodos para las "Bases de Datos" como: `collapse_4`, `collapse_5`
  div_BaseDatos <- pagina %>%
    rvest::html_nodes("#collapse_4, #collapse_5")
  # rvest::html_nodes(xpath = '//*[@id="collapse_4" or @id="collapse_5"]')
  # Recuperamos los enlaces de descarga
  links_BaseDatos <-
    rvest::html_nodes(div_BaseDatos,"a") %>%
    rvest::html_attr("href")
  
  cat("\nVínculos rescatados de [",link,"]\n")
  print(links_BaseDatos)
  
  return(links_BaseDatos)
}

obtencionEnlacesDescarga <- function(enlaces_descarga, identificador) {
  
  enlaces <- enlaces_descarga
  
  # Paquete para analisis de texto
  if (!require("httr")) { 
    install.packages("httr")
    library(httr)
  }
  
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
    barraProgreso(enlaces)
    cat("Del vínculo:\n\t[", enlace, "]\nse ha capturado la ruta de descarga:\n\t[", head$url, "].\n")
    #cat(paste0("\n[",k,"]"),"Del vínculo:\n\t[", enlace, "]\nse ha capturado la ruta de descarga:\n\t[", head$url, "].\n")
  }
  
  informacion <-
    data.frame(
      time = Sys.time(),
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
  
  #actualizarReporte(dataFrame = informacion, nombre_archivo = paste("Reporte Enlaces de Descarga", identificador))
  
  return(informacion)
}

descargaArchivosEnlacesAnalizados <- function(enlaces, informacion, ruta_destino) {
  
  desc_dir <- file.path(ruta_destino)
  
  crearDirectorio(desc_dir)
  
  url <- informacion$url
  
  status <- informacion$status_code
  
  if (exists("contador_progreso")) rm(contador_progreso, envir = .GlobalEnv)
  
  for ( k in seq_along(url) ) {
    barraProgreso(seq_along(url))
    dest_file <- file.path(desc_dir, basename(url[k]))
    if ( (!file.exists(dest_file) || grepl(format(Sys.Date(), "%Y"),url[k]) ) && status[k] == 200 ) {
      # El argumento `timeout = 120` indica que R esperará hasta 120 segundos antes de cancelar la descarga si no recibe una respuesta del servidor.
      download.file(url[k], dest_file, timeout = 300)
    } else if ( file.exists(dest_file) ) {
      cat("El archivo: [", basename(url[k]), "] ya existe en el directorio: [", dest_file, "].\n")
    } else if ( status[k] == 404 ) {
      cat("\nEl archivo: [", basename(url[k]), "], NO está disponible en la dirección: [", url[k], "].\n")
    }
  }
}

gestorDescargasSEPS <- function() {
  
  enlaces_SEPS <- analisisVinculosPaginaWebSEPS()
  
  info_enlaces_SEPS <- obtencionEnlacesDescarga(enlaces_descarga = enlaces_SEPS, identificador = "SEPS")
  
  ruta_descargas_SEPS <- "data/Descargas/SEPS/Bases de Datos/Estados Financieros"
  descargaArchivosEnlacesAnalizados(enlaces_SEPS, info_enlaces_SEPS, ruta_descargas_SEPS)
  
  ruta_fuentes_SEPS <- "data/Fuente/SEPS/Bases de Datos/Estados Financieros"
  descompresionArchivosDirectorio(ruta_descargas_SEPS, ruta_fuentes_SEPS)
  
}

# SEPS----

modificadorNombreColumna <- function(tabla, nombre_nuevo, ...) {
  
  # nombre_nuevo: debe ser un string con el nombre deseado para homogeneizar
  
  # Lista de palabras a buscar por columna
  palabras <- list(...)
  # Buscamos las palabras clave en los nombres de las variables
  coincidencias <- lapply(palabras, function(x) stringr::str_detect(names(tabla), stringr::regex(x, ignore_case = TRUE)))
  # Determinamos el nombre de columna a cambiar
  nombre_anterior <- names(tabla)[Reduce(`&`, coincidencias)]
  # Instalación condicional del paquete y llamado del paquete
  if (!require("rlang")) { install.packages("rlang") }
  library(rlang)
  # Se puede pasar el nuevo nombre de la columna como una variable desde fuera de la función dplyr::rename(). Para hacerlo, puedes usar la función rlang::sym() para convertir el valor de la variable en un símbolo y luego usar el operador !! para evaluarlo dentro de dplyr::rename()
  tabla <- dplyr::rename(tabla, !!rlang::sym(nombre_nuevo) := nombre_anterior)
  # Resultado de la función
  return(tabla)
}

generarListaTablasSEPS <- function() {
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  directorio_principal <- "data/Fuente/SEPS/Bases de Datos"
  archivos <- list.files(path = directorio_principal, full.names = TRUE, recursive = TRUE)
  lista_tablas_SEPS <- list()
  for ( archivo in archivos ) {
    barraProgreso(archivos)
    cat("Impotando y procesando: [", normalizePath(archivo), "]\n")
    nombre_tabla <- basename(dirname(archivo))
    lista_tablas_SEPS[[nombre_tabla]] <-
      archivo %>%
      readr::read_delim(., guess_max = 1000 ) %>%
      modificadorNombreColumna(., nombre_nuevo = "FECHA", "fecha", "corte") %>%
      modificadorNombreColumna(., nombre_nuevo = "SEGMENTO", "segmento") %>%
      modificadorNombreColumna(., nombre_nuevo = "RUC", "ruc") %>%
      modificadorNombreColumna(., nombre_nuevo = "RAZON_SOCIAL", "razon", "social") %>%
      modificadorNombreColumna(., nombre_nuevo = "DESCRIPCION", "descripcion", "cuenta") %>%
      modificadorNombreColumna(., nombre_nuevo = "CODIGO", "cuenta") %>%
      modificadorNombreColumna(., nombre_nuevo = "CUENTA", "descripcion") %>%
      modificadorNombreColumna(., nombre_nuevo = "VALOR", "saldo") %>%
      dplyr::mutate(
        `FECHA` = lubridate::ymd(`FECHA`),
        `SEGMENTO` = as.character(`SEGMENTO`),
        `RUC` = as.character(`RUC`),
        `RAZON_SOCIAL` = as.character(`RAZON_SOCIAL`),
        `CUENTA` = as.character(`CUENTA`),
        `CODIGO` = as.integer(`CODIGO`),
        `VALOR` = as.numeric(gsub(",", ".", `VALOR`)),
        `SEGMENTO` = ifelse(`SEGMENTO` == "SEGMENTO 1 MUTUALISTA","MUTUALISTA", `SEGMENTO`)
      )
  }
  return(lista_tablas_SEPS)
}

concatenarTablasSEPS <- function() {
  library(dplyr)
  library(data.table)
  
  tic_general <- Sys.time()
  
  gestorDescargasSEPS()
  
  lista_tablas_SEPS <- generarListaTablasSEPS()
  tabla_concatenada <- dplyr::bind_rows(lista_tablas_SEPS)
  # tabla_concatenada <-
  #   lista_tablas_SEPS %>%
  #   dplyr::bind_rows() %>%
  #   correccionCaracteresParalelizadaRazonSocialSEPS()
  
  tic <- Sys.time()
  cat("\n\nEspere unos minutos, exportando...\n")
  nombre_archivo <- paste0("data/Base de Datos/SEPS Bases de Datos ", max(tabla_concatenada$FECHA), ".csv")
  #write.csv(tabla_concatenada, nombre_archivo, row.names = FALSE)
  data.table::fwrite(tabla_concatenada, nombre_archivo)
  cat("\nSe ha creado el archivo con la ruta: [", normalizePath(nombre_archivo), "]\n")
  cat("\n Duración:", difftime(Sys.time(), tic, units = "mins"), "minutos." )
  
  cat("\n\n Duración total del proceso:", difftime(Sys.time(), tic_general, units = "mins"), "minutos.\n" )
  
  return(tabla_concatenada)
}

# CUNCLUIDO----

verificadorInstalacion <- function(ruta_intalacion) {
  
  # Esta función ejecuta un comando de PowerShell para buscar la ubicación del ejecutable de Excel en el Registro de Windows
  
  # EJEMPLO:
  # ruta_intalacion <- "'HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe'"
  # verificadorInstalacion(ruta_intalacion)
  # verificadorInstalacion("'C:\\Program Files\\7-Zip\\7zFM.exe'")
  
  # Script de Power Shell para verificar instalción en determinada ruta
  script <- paste0("Test-Path ", ruta_intalacion)
  # Ejecutar el script de PowerShell y capturar el resultado
  is_installed <- system2("powershell", script, stdout = TRUE)
  
  return(as.logical(is_installed))
}

verificadorInstalacionWindows <- function(nombre_aplicacion) {
  
  # Verifica si una aplicación de nombre similar está instaldo en el sistema
  
  # EJEMPLOS:
  # nombre_aplicacion <- "7-Zip"
  # nombre_aplicacion <- "Microsoft 365"
  # verificadorInstalacionWindows(nombre_aplicacion)
  # verificadorInstalacionWindows("Zip")
  # verificadorInstalacionWindows("Office")
  # verificadorInstalacionWindows("Microsoft 365")
  
  # Comando de PowerShell que busca en el registro de Windows para encontrar entradas relacionadas con `nombre_aplicacion` en la clave Uninstall
  script <- paste0("if (Get-ItemProperty HKLM:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Where-Object {$_.DisplayName -like '*",
                   nombre_aplicacion,
                   "*'}) { $true } else { $false }")
  # Ejecutamos el comando en el script
  is_installed <- system2("powershell", script, stdout = TRUE)
  # Devolvemos un valor lógico en R
  is_installed <- as.logical(is_installed)
  # Si la prueba es afirmativa agregamos atributos realiacionados a la aplicación
  if (is_installed) {
    # Comando de PowerShell para obtener el nombre y la versión de la instalación
    script <- paste0("Get-ItemProperty HKLM:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Where-Object {$_.DisplayName -like '*",
                     nombre_aplicacion,
                     "*'} | Select-Object DisplayName, DisplayVersion, InstallLocation | Format-List")
    # Almacenamos la información recuperada de Powershell
    installed_version <- system2("powershell", script, stdout = TRUE)
    installed_version <- installed_version[installed_version != ""]
    output_df <- data.frame(matrix(gsub(".*: ", "", installed_version), ncol = 3, byrow = TRUE))
    colnames(output_df) <- c("DisplayName", "DisplayVersion", "InstallLocation")
    # Agregamos el atributo al valor lógico anterior
    attr(is_installed, "info") <- output_df
  }
  
  return(is_installed)
  
}

executaInstalardor7zip <- function() {
  
  # Verifica si 7zip está instalado y si no lo está, ejecuta el instalador la aplicación almacenado en el directorio del proyecto
  
  # EJEMPLO: executaInstalardor7zip()
  
  # Define el nombre del archivo de instalación
  ruta_7zip <- "installers/7z2201-x64.exe"
  # Define la ruta del archivo de instalación
  file_path <- normalizePath(ruta_7zip)
  # Verifica si 7-Zip está instalado
  #prueba_instalacion <- verificadorInstalacion("'C:\\Program Files\\7-Zip\\7zFM.exe'")
  prueba_instalacion <- verificadorInstalacionWindows("7-Zip")
  # Instala 7-Zip si no está instalado
  if ( !prueba_instalacion ) {
    # Scrip Powrshell para executar el instador de 7zip
    script <- paste0("Start-Process '", file_path, "' -ArgumentList '/S' -Verb runAs")
    # Instala 7-Zip usando PowerShell
    system2("powershell", script)
  }
  
}

descompresion7zip <- function(ruta_archivo_normalizado, ruta_destino_normalizado) {
  
  # Esta función permite descomprimir un archivo .zip empleando 7-Zip ejecutado desde Powershell
  
  # EJEMPLO:
  # ruta_archivo_normalizado <- "D:\\INNOVACION\\PASANTE\\DESARROLLO\\BalanceFinacieroSFN\\data\\Descargas\\SB\\Boletin Financiero Mensual\\PUBLICA\\2006\\BOL_FIN_PUB_ENE_06.zip"
  # ruta_destino_normalizado <- "C:\\Users\\PASANTE.INTELIGENCIA\\Downloads"
  # descompresion7zip(ruta_archivo_normalizado, ruta_destino_normalizado)
  
  procedimiento_descompresion <- function() {
    # Recuperamos información de la instalación de 7-Zip
    info_instalacion_7zip <- attr(prueba_instalacion_7zip,"info")
    # Ruta de instalción de 7-Zip
    ruta_instalacion_7zip <- info_instalacion_7zip$InstallLocation
    # Ruta del ejecutable de 7-Zip
    ruta_ejecutable_7zip <- paste0(ruta_instalacion_7zip, "7z.exe")
    # Script para descomprime el archivo zip utilizando 7-Zip a través de Powershell
    script <- paste0("& '", ruta_ejecutable_7zip, "'", " x ", 
                     "'", ruta_archivo_normalizado, "'", " -o", 
                     "'", ruta_destino_normalizado, "'")
    # Ejecucion del script con Powershell
    system2("powershell", script, stdout = TRUE)
  }
  
  # Verificamos la instalación de 7-Zip
  prueba_instalacion_7zip <- verificadorInstalacionWindows("7-Zip")
  
  if ( prueba_instalacion_7zip == TRUE ) {
    # Realizamos la descompresión
    procedimiento_descompresion()
  } else {
    # Instalamos 7-Zip
    executaInstalardor7zip()
    # Realizamos la descompresión
    procedimiento_descompresion()
  }
  
}

descompresionArchivosDirectorio <- function(origen, destino) {
  
  # Esta función permite descomprimir directorios con archivos `.zip`
  
  # EJEMPLO:
  # origen <- "data/Descargas/SEPS/Bases de Datos"
  # destino <- "data/Fuente/SEPS/Bases de Datos"
  # descompresionArchivosDirectorio(origen, destino)
  
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
      descompresionArchivosDirectorio(ruta_origen_temporal,ruta_destino)
      #
      origen <- ruta_origen_temporal
      destino <- ruta_destino
      descompresionArchivosDirectorio(origen,destino)
      #
      # Elimina los archivos comrpimidos temporales
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
      # cat(paste0("\n[",k,"]"),"Copiando el archivo: [", normalizePath(ruta_origen),"] ...\n")
      # file.copy( ruta_origen, file.path(destino, archivo) )
      copiado_archivo()
    }
  }
  
  # Establecer el directorio de destino para los archivos Descomprimidos
  crearDirectorio(destino)
  # Listo los archivos del directorio
  archivos <- list.files(origen, recursive = TRUE)
  # Descompresión de archivos
  k <- 0
  for ( archivo in archivos ) {
    k <- k + 1
    ruta_origen <- file.path( origen, archivo )
    descompresion_zip()
    barraProgreso(archivos)
  }
}

cerrarLibroExcel<- function(ruta_libro) {
  
  # EJMPLO:
  # ruta_libro <- "D:\\INNOVACION\\PASANTE\\DESARROLLO\\BalanceFinacieroSFN\\data\\mtcars.csv"
  # cerrarLibroExcel(ruta_libro)
  
  # Cerrarmos toda la aplicación
  # Get-Process excel | Foreach-Object { $_.CloseMainWindow() }
  
  ruta_libro_normalizada <- normalizePath(ruta_libro)
  
  if ( length(ruta_libro_normalizada) < 1  ) {
    cat("\nIngrese una ruta válida")
  } else if ( length(ruta_libro_normalizada) == 1 ) {
    rutas_cerrar <- paste0("'",ruta_libro_normalizada,"'")
  } else {
    rutas_cerrar <- paste(paste0("'",ruta_libro_normalizada,"'"), collapse = ",")
  }
  # Script para cerrar el libro de Excel especificado empleando Powershell
  script <- paste0("$filesToClose = @(", rutas_cerrar, ");",
                   " ([Runtime.Interopservices.Marshal]::GetActiveObject('Excel.Application')).Workbooks",
                   " | Where-Object { $filesToClose -contains $_.FullName }",
                   " | ForEach-Object { $_.Close() }")
  # Ejecucion del script con Powershell
  system2("powershell", script)
}

xlsb2xlsx <- function(ruta_archivo_xlsb) {
  
  # Esta función permite transformar el formato de un archivo de Excel con extención ".xlsb" a ".xlsx" y reemplazarlo empleando Windows PowerShell
  
  # EJEMPLO:
  # ruta_archivo_xlsb <- "data/Fuente/Casos Particulares/BOL_FIN_PUB_SEPT_20.xlsb"
  # xlsb2xlsx(ruta_archivo_xlsb)
  
  verificadorExcel <- function() {
    # Ruta genérica para que PowerShell encuentre a Excel
    ruta_excel <- "'HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe'"
    return ( verificadorInstalacion(ruta_excel) )
  }
  verificadorArchivo <- function(ruta_archivo_xlsb) {
    if (file.exists(ruta_archivo_xlsb)) {
      return(TRUE)
    } else {
      cat(paste("\nLa ruta no existe\n"))
      return(FALSE)
    }
  }
  verificadorFormato_xlsb <- function(ruta_archivo_xlsb) {
    # Requerimiento de paquetes
    if (!require("tools")) {
      install.packages("tools")
      library(tools)
    }
    
    if (tools::file_ext(ruta_archivo_xlsb) == "xlsb") {
      return(TRUE)
    } else {
      cat(paste("\nEl archivo con ruta: [", ruta_archivo_xlsb,"],",
                "no es de formato \".xlsb\" por lo que no se realizaron cambios\n"))
      return(FALSE)
    }
    
  }
  trasnformador2xlsb <- function(ruta_archivo_xlsb) {
    # Ruta del archivo xlsb
    xlsbFile <- normalizePath(ruta_archivo_xlsb)
    
    # Ruta del archivo xlsx
    xlsxFile <- normalizePath(gsub(".xlsb", ".xlsx",ruta_archivo_xlsb))
    
    # Crear el script de PowerShell
    script <- paste(
      # Ruta del archivo xlsb
      paste0("$xlsbFile = ", '"', xlsbFile, '"'),
      # Ruta del archivo xlsx
      paste0("$xlsxFile = ", '"', xlsxFile, '"'),
      # Crear un objeto COM de Excel
      "$excel = New-Object -ComObject Excel.Application",
      # Deshabilitar las alertas
      "$excel.DisplayAlerts = $false",
      # Abrir el archivo xlsb
      "$workbook = $excel.Workbooks.Open($xlsbFile)",
      # Guardar como archivo xlsx, donde 1 corresponde al formato xls y 51 a xlsx
      "$workbook.SaveAs($xlsxFile, 51)",
      # Cerrar el libro y salir de Excel
      "$workbook.Close()",
      "$excel.Quit()",
      sep = "\n"
    )
    
    # Guardar el script en un archivo en el directorio principal
    writeLines(script, "convert.ps1")
    
    # Mensaje
    cat(paste("\nSe remplazo el archivo \".xlsb\" de ruta: [", xlsbFile,"],",
              "con el archivo \".xlsx\" de ruta [", xlsxFile,"]\n"))
    
    # Ejecutar el script de PowerShell
    shell("powershell -File convert.ps1", wait = TRUE)
    
    # Eliminar el archivo de script y el .xlsb original
    file.remove("convert.ps1",xlsbFile)
  }
  
  # Condiciones no admisibles
  if ( verificadorArchivo(ruta_archivo_xlsb) && verificadorFormato_xlsb(ruta_archivo_xlsb)) {
    if ( verificadorExcel() ) {
      trasnformador2xlsb(ruta_archivo_xlsb)
    } else {
      cat("\nRequiere instalación de Microsoft Excel")
    }
  }
  
}

frecuenciaEmpiricaRelativaOcurrenciaDecimalTexto <- function(cadena_texto) {
  
  # Esta función calcula la frecuencia relativa de la ocurrencia de la expresión de un número decimal en una columna.
  
  # cadena_texto: es un vector tipo char que puede contener diferentes tipos de información todos convertidos en carácteres.
  
  # Ejemplo: frecuenciaEmpiricaRelativaOcurrenciaDecimalTexto(c("a",1,3.14,Sys.Date())) devolverá 0.25 como la frecuencia relativa por le 3.14
  
  # Configuramos el objeto leído
  
  # Determinamos expresiones regulares para excluir expresiones con puntos y solo admitir con números
  expresion_regular_codigo <- "^[[:digit:]]{1,6}$"
  # Determinamos expresiones regulares para identificar números con o sin decimales, positivos o negativos, con o sin notación científica
  #expresion_regular_numero <- "^[0-9]+([.,][0-9]+)?$"
  #expresion_regular_numero <- "^[-]?[0-9]+([.,][0-9]+)?(E-?[0-9]+)?$"
  expresion_regular_numero <- "^[-]?[0-9]+([.,][0-9]+)?([Ee][-+]?[0-9]+)?$"
  # Determinamos expresiones regulares para para el cero como palabra completa
  expresion_regular_cero <- "^[0]?$"
  # Determinamos la prueba lógica para números decimales excluyendo los enteros pero aceptando el cero y NA
  prueba_numero_decimal <- 
    ( !grepl(expresion_regular_codigo, cadena_texto) & 
        grepl(expresion_regular_numero, cadena_texto) ) |
    grepl(expresion_regular_cero, cadena_texto) |
    is.na(cadena_texto)
  # Asumiendo distribución Poisson para cada caso, calculamos un estimador para la media que representa la tasa media de ocurrencia
  frecuencia_relativa_ocurrencia <- mean(prueba_numero_decimal, na.rm = TRUE)
  
  return(frecuencia_relativa_ocurrencia)
}

indicePrimeraFilDecimalTabla <- function(tabla) {
  
  frecuencia_ocurrencia_decimal_filas <-
    sapply(
      1:nrow(tabla),
      function(fila) {
        cadena_texto <- as.character(tabla[fila,])
        frecuenciaEmpiricaRelativaOcurrenciaDecimalTexto(cadena_texto)
      }
    )
  # diferencia_absoluta <- abs(diff(frecuencia_ocurrencia_decimal_filas))
  # diferencia_maxima <- max(diferencia_absoluta)
  # intersect(which(diferencia_absoluta == diferencia_maxima), which.min(frecuencia_ocurrencia_decimal_filas)) + 1
  primera_fila_decimal <- which.min(frecuencia_ocurrencia_decimal_filas) + 1
  
  return(primera_fila_decimal)
}

analisisDifusoNLPFechaCorte <- function(tabla) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.
  
  # Paquete para manejo de fechas
  if (!require("lubridate")) { 
    install.packages("lubridate")
    library(lubridate)
  }
  
  # Paquete para procesamiento de lenguaje natural
  if (!require("parsedate")) { 
    install.packages("parsedate")
    library(parsedate)
  }
  
  # Paquete para analisis de texto
  if (!require("stringdist")) { 
    install.packages("stringdist")
    library(stringdist)
  }
  
  traductor_mes <- function(texto) {
    
    # Esta función modifica con la traducción al ingles correspondiente sean los nombres completos o las abreviaciones de los meses, para un posterior reconocimiento optimo de fecha
    
    texto_original <- tolower(texto)
    # Creamos un diccionario para traducción y posterior reconocimiento optimo de fechas
    meses <-
      data.frame(
        es = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"),
        #en = substr(strsplit(tolower(month.name), " "), 1, 3)
        en = tolower(month.name)
      )
    # Definimos el patrón buscado en el texto
    patron <- meses$es
    # Definimos el texto de reemplazo
    reemplazo <- meses$en
    # Definimos los separadores admisibles para las palabras
    separadores <- "[-,/, ]"
    # Separamos cada palabra en sus letras componentes
    palabras <- strsplit(texto_original, separadores)[[1]]
    # palabras <- unlist(strsplit(texto_original, separadores))
    # Elegimos únicamente las 3 primeros caracteres de cada palabra para obtener expresiones como: "ene"
    palabras_abreviadas <- substr(palabras, 1, 3)
    # Calculamos las similitudes entres las palabras abreviadas y el patrón de busqueda
    similitudes <- stringdist::stringsimmatrix(palabras_abreviadas, patron, method = "jw")
    # Se emplea una probabilidad de similitud del 90% para compensar el error por identidad con el máximo
    #posiciones_max <- as.data.frame(which(similitudes >= 0.8*max(similitudes), arr.ind = TRUE))
    # Buscamos los índices con las mayores coincidencias
    posiciones_max <- as.data.frame(which(similitudes == max(similitudes), arr.ind = TRUE))
    # Determinamos las abreviaciones similares
    palabra_similar <- palabras[posiciones_max$row]
    # Reemplazamos con la palabra completa las abreviaciones similares
    reemplazo_similar <- reemplazo[posiciones_max$col]
    # Modificamos uno a uno los nombres de los mes traducidos
    texto_modificado <- texto_original
    for ( k in seq_along(palabra_similar) ) {
      texto_modificado <- gsub(palabra_similar[k], reemplazo_similar[k], texto_modificado)
    }
    return(texto_modificado)
  }
  prueba_anio <- function(texto) {
    # Año actual a texto, para generar expresión regular de año, usan Sys.Date() y descomponiéndolo
    anio_num <- year(Sys.Date())
    anio_text <- strsplit(as.character(anio_num), split = "")[[1]]
    # Establecemos una expresión regular que acepta 2000 hasta el año actual
    expresion_regular_anio <- paste0("\\b(",anio_text[1],"[",0,"-",anio_text[2],"][",0,"-",anio_text[3],"][0-9])\\b")
    # expresion_regular_anio <- "\\b(20[0-3][0-9])\\b$" # acepta desde 2000 hasta 2039
    return(grepl(expresion_regular_anio, texto, ignore.case = TRUE))
  }
  prueba_mes <- function(texto) {
    # Establecemos una expresión regular
    expresion_regular_mes <- paste0(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"), collapse = "|")
    return(grepl(expresion_regular_mes, texto, ignore.case = TRUE))
  }
  prueba_dia <- function(texto) {
    # Establecemos una expresión regular del día del mes
    expresion_regular_dia <- "\\b([1-2]?[0-9]|3[0-1])\\b"
    return(grepl(expresion_regular_dia, texto, ignore.case = TRUE))
  }
  formato_numerico_excel <- function(fecha) {
    # Función para transformar a formato numérico de Excel una fecha date
    # Fecha base de Excel
    fecha_base_excel <- as.Date("1899-12-30")
    return(as.numeric(difftime(as.Date(fecha), as.Date("1899-12-30"))))
  }
  prueba_fecha_excel <- function(texto) {
    # Fecha de inicio de busqueda en formato numérico de Excel
    fecha_num_excel_inicio <- formato_numerico_excel("2000-01-01")
    # Fecha de inicio descompuesta en caracteres para formar expresión regular
    fechaI <- strsplit(as.character(fecha_num_excel_inicio), split = "")[[1]]
    # Fecha de actual en formato numérico de Excel para busqueda
    fecha_num_excel_fin <- formato_numerico_excel(Sys.Date())
    # Fecha de fin descompuesta en caracteres para formar expresión regular
    fechaF <- strsplit(as.character(fecha_num_excel_fin), split = "")[[1]]
    # Establecemos una expresión regular que acepte los formatos numéricos para fecha de Excel
    expresion_regular_fecha_num_excel <-
      paste0(
        "^(",fechaI[1],"[",fechaI[2],"-9]","[",fechaI[3],"-9]","[",fechaI[4],"-9]","[",fechaI[5],"-9]|",
        fechaF[1],"[0-9]{", length(fechaF)-1, "})"
      )
    return(grepl(expresion_regular_fecha_num_excel, texto))
  }
  prueba_fecha_date <- function(texto) {
    # Establecemos una expresión regular que acepte variantes de formato fecha
    expresion_regular_fecha_date <-
      paste(c(
        "\\b(20[0-9]{2}[-/][0-1][0-9][-/][0-3]?[0-9])\\b",
        #"\\b(20[0-9]{2}[-/][[:alpha:]]{1,10}[-/][0-3]?[0-9])\\b",# NO USAR ALTERA EN EL CONDICIONAL
        "\\b([0-3]?[0-9][-/][0-1][0-9][-/]20[0-9]{2})\\b" #,
        #"\\b([0-3]?[0-9][-/][[:alpha:]]{1,10}[-/]20[0-9]{2})\\b"
      ), collapse = "|"
      )
    return(grepl(expresion_regular_fecha_date, texto))
  }
  
  # Empleamos la función indicePrimeraFilDecimalTabla() para identificar la primera fila decimal
  indice_fila_nombres <- indicePrimeraFilDecimalTabla(tabla)
  # Subtabla previa a los valores decimales, y a la fila de nombres de columnas, por eso se resta 2
  subtabla <- tabla[1:(indice_fila_nombres-2),]
  # Determinamos las coincidencias en la subtabla
  coincidencias <-
    apply(
      subtabla, 2,
      function(fila) {
        prueba_fecha_date(fila) | prueba_fecha_excel(fila) | (prueba_anio(fila) & prueba_mes(fila)) 
      })
  # Identificamos los indices de las entradas con coincidencias
  indices_celda <- data.frame(which(coincidencias, arr.ind = TRUE))
  # Exigimos que haya al menos un resultado
  if ( length(indices_celda) > 0 ) {
    # Especificamos la primera coincidencia
    contenido_celda <- as.character(subtabla[indices_celda$row[1], indices_celda$col[1]])
  } else {
    cat("\nNo se pudo encontrar una fecha.\n")
    break
  } 
  
  # Establecemos el proceso directo para formatos de fecha
  
  if ( prueba_fecha_date(contenido_celda) ) {
    
    fecha_identificada <- parsedate::parse_date(contenido_celda)
    
    # Establecemos la condición para cuando el texto leído corresponde a fecha en formato numérico de Excel
    
  } else if ( prueba_fecha_excel(contenido_celda) ) {
    
    # Determinamos el valor de la celda buscada con la fecha de corte
    num_fecha_corte <- as.numeric(contenido_celda)
    # Determinamos la fecha de corte
    fecha_identificada <- as.Date( num_fecha_corte, origin = "1899-12-30")
    
    # Establecemos el procedimiento para el caso de tener un mes y un año reconocibles
    
  } else if ( prueba_anio(contenido_celda) & prueba_mes(contenido_celda) ) {
    
    # Dividimos el texto original en sus componentes por si hubiera más de una fecha
    texto_dividido <- unlist(strsplit(contenido_celda, " "))
    # Establecemos el proceso cuando haya solo un año, solo un mes, y no más de un día del mes
    if ( sum(prueba_anio(texto_dividido)) == 1 & 
         sum(prueba_mes(texto_dividido)) == 1 & 
         sum(prueba_dia(texto_dividido)) <= 1 ) {
      fechas_reconocidas <- traductor_mes(contenido_celda)
      fecha_identificada <- parsedate::parse_date(fechas_reconocidas)
      # Establecemos el proceso cuando hay más de una fecha en la celda elegida
    } else {
      # Traducimos el contenido de la celda elegida
      fechas_reconocidas <- traductor_mes(contenido_celda)
      # Empleamos un selector para el separador de frases, según formato
      separadores_fechas <-
        if ( grepl("-",contenido_celda, ignore.case = TRUE) ) {
          " "
        } else if ( grepl("de",contenido_celda, ignore.case = TRUE) ) {
          c(" al "," hasta ")
        }
      # Separamos las diferentes frases relacionadas a fechas
      fechas_reconocidas <- strsplit(fechas_reconocidas, separadores_fechas)[[1]]
      # Agregamos un filtro para evitar frases sin el año
      fechas_reconocidas <- fechas_reconocidas[prueba_anio(fechas_reconocidas)]
      fechas_reconocidas <- parsedate::parse_date(fechas_reconocidas)
      # Agregamos un filtro para elegir siempre la mayor de las fechas
      fecha_identificada <- fechas_reconocidas[which.max(fechas_reconocidas)]
    }
  }
  
  # Determinamos el año
  anio <- format(fecha_identificada, "%Y")
  # Determinamos el mes
  mes <- format(fecha_identificada, "%m")
  # Determinamos una fecha preliminar
  fecha_corte_preliminar <- paste(anio,mes,"01",sep = "-")
  # Determinamos el último día del respectivo mes
  fecha_corte <- as.Date(fecha_corte_preliminar) + months(1) - days(1)
  
  return(fecha_corte)
}

modificadorNombresColumnasTablasIF <- function(catalogo = NULL, tabla) {
  
  # Función para identificar, modificar y eliminar los nombres de las columnas de una tabla utilizando un catálogo de operadores y el método de similitud de cadenas Jaro-Winkler.
  
  # Ejemplo de uso: tabla <- modificadorNombresColumnasTablasIF(tabla = tabla)
  
  if ( is.null(catalogo) ) {
    # Requerimiento de paquetes
    if (!require("readxl")) { 
      install.packages("readxl")
      library(readxl)
    }
    # Leemos el catálogo de una libro de Excel
    catalogo <- readxl::read_excel("data/Otros/Catálogo Operadores.xlsx")
    # Se agrega 0 como número y "0" como carácter para nombres inadecuados en columnas
    catalogo_complementario <- data.frame(
      RUC = rep("0"),
      Operadora = c("FECHA", "CODIGO", "CUENTA",
                    "BANCA MULTIPLE",
                    "BANCOS PRIVADOS GRANDES",
                    "BANCOS PRIVADOS MEDIANOS",
                    "BANCOS PRIVADOS PEQUEÑOS",          
                    "BANCOS PRIVADOS COMERCIALES",
                    "BANCOS PRIVADOS CONSUMO",      
                    "BANCOS PRIVADOS VIVIENDA",
                    "BANCOS PRIVADOS DE MICROEMPRESA",
                    "BANCOS PRIVADOS DE MICROCREDITO",
                    "TOTAL BANCOS PRIVADOS",
                    "INSTITUCIONES FINANCIERAS DE PRIMER PISO",
                    "INSTITUCIONES FINANCIERAS DE SEGUNDO PISO",
                    "TOTAL BANCA PÚBLICA"))
    catalogo <- rbind(catalogo_complementario, catalogo)
  }
  
  # Requerimiento de paquetes
  if (!require("stringdist")) { 
    install.packages("stringdist")
    library(stringdist)
  }
  # Obtener los nombres de las columnas de la tabla
  nombres_columnas <- colnames(tabla)
  # Calcular la similitud de cadenas entre los elementos del catálogo y los nombres de las columnas
  distancia <- as.data.frame(stringdist::stringsimmatrix(catalogo$Operadora, nombres_columnas, method = "jw" ))
  # Colocar los nombres de las filas y columnas en la tabla generada
  colnames(distancia) <- nombres_columnas
  rownames(distancia) <- catalogo$Operadora
  # Identificar el nombre del catálogo con la mayor similitud para cada columna
  identificacion <- sapply(seq_along(distancia), function(columna) row.names(distancia)[which.max(distancia[[columna]])])
  #View(data.frame(original = nombres_columnas, identificacion = identificacion))
  # Renombrar las columnas de la tabla con los nombres identificados
  names(tabla) <- identificacion
  
  return(tabla)
}

nombreHojaSimilar <- function(ruta_libro, nombre_hoja_buscado) {
  
  # Determina la hoja en un libro de excel que tiene mayor similitud a la hoja buscada
  
  nombres_hojas <- readxl::excel_sheets(ruta_libro)
  distancia <- stringdist::stringsimmatrix(nombre_hoja_buscado, nombres_hojas, method = "jw")
  indice_hoja_similar <- which.max(distancia)
  nombre_hoja_similar <- nombres_hojas[indice_hoja_similar]
  return(nombre_hoja_similar)
}

hojaToTablaBoletinesFinancierosSB <- function(ruta_libro, nombre_hoja, fecha_corte = NULL) {
  
  # Esta función permite extraer la tabla de datos contenida en un hoja de cálculo correspondiente a los "Boletines Financieros mensuales" de la SB
  
  # ARGUMENTOS:
  # ruta_libro <- "data/Fuente/SB/PRIVADA/2023/FINANCIERO MENSUAL BANCA PRIVADA 2023_02.xlsx"
  # nombre_hoja <- "BALANCE"
  # fecha_corte <- "2023-02-29"
  # EJEMPLO: tabla <- hojaToTablaBoletinesFinancierosSB(ruta_libro, nombre_hoja, fecha_corte)
  
  # Requerimiento de paquetes
  if (!require("readxl")) {
    install.packages("readxl")
    library(readxl)
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  # Determinamos el nombre de hoja con mayor similitud al buscado
  nombre_hoja <- nombreHojaSimilar(ruta_libro, nombre_hoja)
  # Importamos las 30 primeras filas de una hoja específica de un libro de excel en una ruta determinada
  hoja <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = FALSE, n_max = 30))
  # Regla de decisión para la fecha de corte
  fecha_corte <-
    if ( is.null(fecha_corte) ) {
      # Determinamos la fecha más probable contenida en la hoja importada
      analisisDifusoNLPFechaCorte(hoja)
    } else {
      fecha_corte
    }
  # Determinamos la fila más probable con los nombres de las columnas
  indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
  # Almacenamos la fila con los nombres de las columnas
  nombres_columnas <- unname(unlist(hoja[indice_fila_nombres_columnas,]))
  # Importamos una tabla de prueba para verificar la correcta asignación de los nombres de las columnas en sus 20 primeras filas
  tabla_prueba <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20))
  # Verificamos si coinciden adecuadamente los nombres de las columnas
  if ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 ) {
    # Retrocedemos un índice en las filas previo a iterear para incluir cualquier caso exepcional
    indice_fila_nombres_columnas <- indice_fila_nombres_columnas - 2
    # Iteramos hasta que hayan coincidencias en al menos el 80%
    while ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 & indice_fila_nombres_columnas <= 20 ) {
      # Incrementamos el índice de la fila para continuar la prueba
      indice_fila_nombres_columnas <- indice_fila_nombres_columnas + 1
      # Reimportamos la tabla de prueba para verificar la correcta asignación de los nombres de las columnas en sus 20 primeras filas
      tabla_prueba <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20))
    }
  }
  # Inicializamos la variable para almacenar la advertencias
  advertencias <- NULL
  # Volvemos a importar la hoja de cálculo pero especificando la fija de inicio, para que se reconozca el tipo de dato y nombre de cada columna
  tabla <-
    # Usamos withCallingHandlers() para capturar las advertencias generadas durante la ejecución del código y almacenarlas en una variable
    withCallingHandlers(
      # Importamos únicamente la tabla de datos contenida en la hoja especificada, saltando las primeras filas
      suppressMessages(
        readxl::read_excel(ruta_libro,
                           sheet = nombre_hoja,
                           col_names = TRUE,
                           skip = indice_fila_nombres_columnas)),
      # Empleamos una función como manejador de advertencias
      warning = function(w) {
        # La función toma un argumento w, que es un objeto de advertencia que contiene información sobre la advertencia generada
        advertencias <<- c(advertencias, w$message)
        # Suprimimos la advertencia y evitamos que la advertencia se muestre en la consola y permite que el código continúe ejecutándose normalmente
        invokeRestart("muffleWarning")
      }
    )
  # Agregamos las advertencias como un atributo de la tabla
  attr(tabla, "advertencias") <- advertencias
  # Agregamos la columna con la fecha del "Boletín Financiero mensual"
  tabla <-
    tabla %>%
    # Eliminamos las columnas que no contengan caracteres alfabéticos
    select( -matches("^[^[:alpha:]]+$", .) ) %>%
    # Eliminamos las filas que contienen únicamente valores NA
    filter( !if_all(everything(), is.na) ) %>%
    # Empleamos la función creada para modificar los nombres de las columnas según un catálogo por defecto
    #
    modificadorNombresColumnasTablasIF(tabla = .) %>%
    #
    # Modificamos la columna CODIGO a texto
    mutate(CODIGO = as.character(CODIGO)) %>%
    # Modificamos la columna CUENTA a texto
    mutate(CUENTA = as.character(CUENTA)) %>%
    # Modificamos el resto de columnas a numéricas
    mutate_at(vars(-CODIGO, -CUENTA), as.numeric) %>%
    # Eliminamos todas las filas donde el valor en las columnas "CODIGO" y "CUENTA" es NA
    filter( !(is.na(CODIGO) & is.na(CUENTA)) ) %>%
    # Eliminamos las filas donde todas las columnas son NA excepto CUENTA
    filter( !if_all(-CUENTA, is.na) ) %>%
    # Eliminamos las filas donde la columna CODIGO tenga letras mientras todas las las demás columnas son NA
    filter( !(grepl("[[:alpha:]]+",CODIGO) & if_all(-CODIGO, is.na)) ) %>%
    # Agregamos la columna con la fechas de corte
    mutate(`FECHA` = rep(fecha_corte)) %>%
    # Movemos la columna FECHA al inicio de la tabla
    select(`FECHA`, everything())
  # Agregamos metadatos como atributo de la tabla
  #attr(tabla, "fecha_creacion") <- Sys.Date()
  
  return(tabla)
}

compilarHojasBalanceFinancieroSB <- function(ruta_directorio = NULL) {
  
  # Esta función realiza todo el proceso necesario para crear la base de datos de los Balances Financieros mensuales de la SB
  
  # Requerimiento de paquetes
  if (!require("readxl")) { 
    install.packages("readxl")
    library(readxl)
  }
  
  # Requerimiento de paquetes
  if (!require("dplyr")) { 
    install.packages("dplyr")
    library(dplyr)
  }
  
  # Requerimiento de paquetes
  if (!require("reshape2")) { 
    install.packages("reshape2")
    library(reshape2)
  }
  
  # Requerimiento de paquetes
  if (!require("purrr")) { 
    install.packages("purrr")
    library(purrr)
  }
  
  # # Cerramos todos los libros de Excel abiertos
  # system2("powershell", "Get-Process excel | Foreach-Object { $_.CloseMainWindow() }")
  # Establecemos la ruta del directorio fuente de los libros de Excel con los "Boletines Financieros mensuales"
  if ( is.null(ruta_directorio) ) ruta_directorio <- "data/Fuente/SB/PRIVADA"
  # Determinamos los archivos presentes en directorio fuente
  archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
  # Determinamos todas las rutas de los archivos en el directorio
  rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  # Determinamos los archivos a transformar de formato
  rutas_transformar <- rutas_libros[tools::file_ext(rutas_libros) == "xlsb"]
  # Realizamos los cambios solo si son necesarios
  if ( length(rutas_transformar) > 0 ) {
    # Cambiar el formato
    purrr::map(rutas_transformar, xlsb2xlsx)
    # Volvemos a determinar los archivos presentes en directorio fuente
    archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
    # Volvemos a determinar todas las rutas de los archivos en el directorio luego del cambio de formato
    rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  }
  # Establecemos una prueba con expresión regular para filtrar los años 2013-2029
  prueba_anio <- grepl("(201[3-9])|(202[0-9])",rutas_libros)
  # Filtramos las rutas con los años establecidos
  rutas_libros_seleccionados <- rutas_libros[prueba_anio]
  # Cerramos todos los libros seleccionados para que no generen ningún error al procesarlos
  cerrarLibroExcel(rutas_libros_seleccionados)
  # Limpiamos la barra de progreso
  if (exists("contador_progreso")) rm(contador_progreso, envir = .GlobalEnv)
  # Inicializamos la lista de las tablas concatenadas de BALANCE y PYG
  lista_tablas_BAL_PYG_concatenadas <- list()
  # Definimos el bucle de ejecución
  for ( ruta_libro in rutas_libros_seleccionados ) {
    # Importamos las 20 primeras filas de la hoja BALANCE para identificar la fecha de corte
    hoja <- suppressMessages(readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
    # Identificamos la fecha de corte
    fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
    # Extraemos la tabla de BALANCE
    tabla_BAL <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte)
    # Extraemos la tabla de PYG
    tabla_PYG <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "PYG", fecha_corte)
    # Definimos el nombre de para cada tabla
    nombre_tabla <- basename(ruta_libro)
    # Asignamos la tabla concatenada de BALANCE y PYG a un elemento de la lista de tablas
    lista_tablas_BAL_PYG_concatenadas[[nombre_tabla]] <- dplyr::bind_rows(tabla_BAL,tabla_PYG)
    # Ejecutamos el código para la barra de progreso
    barraProgreso(rutas_libros_seleccionados)
    # Mostramos la ruta del archivo en proceso
    cat("Procesando el archivo: [", normalizePath(ruta_libro),"]\n")
  }
  # Concatenamos todas las tablas de la lista generada
  tabla_BAL_PYG <- dplyr::bind_rows(lista_tablas_BAL_PYG_concatenadas)
  # Asignamos el registro completo de advertencias (warnings) generadas al convertir a tabla las hojas de cálculo
  registro_advertencias <-
    sapply(seq_along(lista_tablas_BAL_PYG_concatenadas),
           function(k) attr(lista_tablas_BAL_PYG_concatenadas[[k]],"advertencias"))
  # Recuperamos los nombres de cada archivo para el registro de advertencias
  names(registro_advertencias) <- names(lista_tablas_BAL_PYG_concatenadas)
  # Asignamos la información de las advertencias a un data frame
  reporte_consolidacion_BAL_PYG <-
    data.frame(
      Archivo = names(unlist(registro_advertencias)),
      Advertencia = unname(unlist(registro_advertencias)))
  # Exportamos el reporte con el registro de las advertencias
  exportarReporteTabla(reporte_consolidacion_BAL_PYG,
                       paste("Reporte Advertencias en Consolidación Balances Financieros SB",
                             basename(ruta_directorio)))
  # Fundimos (melting) las tablas
  tabla_BAL_PYG_fundida <-
    reshape2::melt(tabla_BAL_PYG,
                   id.vars = colnames(tabla_BAL_PYG)[1:3],
                   variable.name = "RAZON_SOCIAL",
                   value.name = "VALOR")
  
  return(tabla_BAL_PYG_fundida)
}

unionBalanceFinancieroSB <- function() {
  
  # Esta función junta las tablas consolidadas de Balances Financieros tanto de la Banca pública como privada
  
  tic_general <- Sys.time()
  
  # Requerimiento de paquetes
  if (!require("dplyr")) { 
    install.packages("dplyr")
    library(dplyr)
  }
  
  # ETAPA 0: Descompresión de los archivos descargados del portal de la SB ----
  origen <- "data/Descargas/SB/Boletin Financiero Mensual/PUBLICA"
  destino <- "data/Fuente/SB/PUBLICA"
  descompresionArchivosDirectorio(origen, destino)
  
  # ETAPA 1: Consolidación del la tabla de Balance Financiero de la Banca privada ----
  privada <- compilarHojasBalanceFinancieroSB(ruta_directorio = "data/Fuente/SB/PRIVADA") %>%
    mutate(SEGMENTO = "PRIVADA")
  
  # ETAPA 2: Consolidación del la tabla de Balance Financiero de la Banca privada ----
  publica <- compilarHojasBalanceFinancieroSB(ruta_directorio = "data/Fuente/SB/PUBLICA") %>%
    mutate(SEGMENTO = "PUBLICA")
  # publica <- NULL
  
  # ETAPA 3: Concatenación todas de tablas consolidadas ----
  consolidada <- dplyr::bind_rows(privada,publica)
  
  # ETAPA 4: Exportación de base de datos generada ----
  tic <- Sys.time()
  cat("\n\nEspere unos minutos, exportando...\n")
  nombre_archivo <- paste0("data/Base de Datos/SB Boletin Financiero ", max(consolidada$FECHA), ".csv")
  write.csv(consolidada, nombre_archivo, row.names = FALSE)
  cat("\nSe ha creado el archivo con la ruta: [", normalizePath(nombre_archivo), "]\n")
  cat("\n Duración:", difftime(Sys.time(), tic, units = "mins"), "minutos." )
  
  cat("\n\n Duración total del proceso:", difftime(Sys.time(), tic_general, units = "mins"), "minutos.\n" )
  
  return(consolidada)
}
