# General----

requerirPaquetes <- function(...) {
  lista_paquetes_empleados <-
    list(
      "beepr",
      "data.table", "dplyr",
      "httr",
      "lubridate",
      "openxlsx",
      "parallel", "parsedate", "purrr",
      "readr", "readxl", "reshape2", "rlang", "rvest",
      "stats", "stringdist", "stringr",  
      "tools",
      "utils"
    )
  lista_paquetes <-
    if ( length(list(...)) == 0 ) {
      lista_paquetes_empleados
    } else {
      list(...)
    }
  paquetes <- unlist(lista_paquetes)
  for (paquete in paquetes) {
    if ( !require(paquete, character.only = TRUE) ) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  paquetes_desactualizados <- utils::old.packages()
  paquetes_a_actualizar <-
    intersect(
      rownames(paquetes_desactualizados),
      unlist(lista_paquetes_empleados))
  if (length(paquetes_a_actualizar) > 0) {
    update.packages(pkgs = paquetes_a_actualizar, ask = FALSE)
  }
}

crearDirectorio <- function(nueva_ruta) {
  
  # Esta función permite crear cualesquier ruta especificada, dentro del directorio del proyecto.
  
  # EJEMPLO: crearDirectorio("data/Fuente/SB/PRIVADA")
  
  existe_ruta <- dir.exists(nueva_ruta)
  
  if ( !existe_ruta  ) {
    nombre_carpetas_anidadas <- unlist(strsplit(nueva_ruta, "/"))
    for ( k in seq_along(nombre_carpetas_anidadas) ) {
      # Definimos la ruta concatenada
      subdirectorio <- paste(head(nombre_carpetas_anidadas, k), collapse = "/")
      if (!dir.exists(subdirectorio)) {
        dir.create(subdirectorio)
        cat("\n\033[1mSe creo la carpeta:\033[0m [",basename(subdirectorio),"]",
            "con la ruta: [", normalizePath(subdirectorio),"].\n")
      } 
    }
  }
}

exportarReporteTabla <- function(dataFrame, nombre_archivo) {
  requerirPaquetes("openxlsx")
  crear_libro_trabajo <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(crear_libro_trabajo, "Reporte")
  openxlsx::writeData(crear_libro_trabajo, "Reporte", dataFrame) #, autoWidth = TRUE)
  directorio_reportes <- "data/Reportes"
  crearDirectorio(directorio_reportes)
  nombre_archivo <-
    paste(nombre_archivo, "Emitido", format(Sys.time(), "%Y-%m-%d_%HH%M.xlsx"))
  ruta_archivo <- file.path(directorio_reportes, nombre_archivo)
  openxlsx::saveWorkbook(crear_libro_trabajo, ruta_archivo, overwrite = TRUE)
  # if( !dir.exists(ruta_archivo) ) {
  #   openxlsx::write.xlsx(crear_libro_trabajo, ruta_archivo, rowNames = FALSE)
  # }
  cat("\nSe ha creado el archivo con la ruta: [", normalizePath(ruta_archivo), "]\n")
}

actualizarReporte <- function(tabla, nombre_archivo) {
  requerirPaquetes("openxlsx")
  directorio_reportes <- "data/Reportes"
  ruta_reporte <- file.path(directorio_reportes,paste0(nombre_archivo,".xlsx"))
  if ( file.exists(ruta_reporte) ) {
    reporte_anterior <- openxlsx::read.xlsx(ruta_reporte)
    agregar_nueva_fila <- rbind(reporte_anterior, tabla)
  } else {
    agregar_nueva_fila <- tabla
  }
  # Guarda el nuevo data frame en el archivo de Excel
  openxlsx::write.xlsx(agregar_nueva_fila, ruta_reporte)
  cat("\nSe ha actualizado el archivo con la ruta: [", normalizePath(ruta_reporte), "]\n")
}

exportarResultadosCSV <- function(tabla, nombre_archivo, ruta_directorio = NULL) {
  requerirPaquetes("beepr")
  #cat("\n\nExportando resultados...\n")
  cat("\n\n\033[1;32mExportando resultados...\033[0m\n")
  if ( is.null(ruta_directorio) ) {
    ruta_directorio <- "data/Base de Datos"
  }
  nombre_archivo <- paste0(nombre_archivo, " ", max(tabla$FECHA), ".csv")
  ruta_directorio_normalizada <- normalizePath(ruta_directorio)
  dir.create(ruta_directorio_normalizada,recursive = TRUE,showWarnings = FALSE)
  if ( dir.exists(ruta_directorio_normalizada) ) {
    ruta_archivo_normalizada <-
      paste0(ruta_directorio_normalizada, "\\", nombre_archivo)
    data.table::fwrite(tabla, ruta_archivo_normalizada)
    beepr::beep(8)
    cat("\nSe ha creado el archivo con la ruta: [", ruta_archivo_normalizada, "]\n")
  }
}

exportarProyecto <- function() {
  crearDirectorioNormalizado <- function(ruta_directorio) {
    ruta_directorio_normalizada <- normalizePath(ruta_directorio)
    if (!dir.exists(ruta_directorio_normalizada)) {
      dir.create(ruta_directorio_normalizada, recursive = TRUE)
      cat("\nSe creo la carpeta: [", basename(ruta_directorio_normalizada), "]",
          "con la ruta: [", ruta_directorio_normalizada, "].\n")
    }
  }
  copiarProyecto <- function(ruta_destino) {
    directorio_proyecto <- "DESARROLLO\\BalancesEstadosFinacierosSFN"
    ruta_directorio_proyecto <-
      paste(ruta_directorio_compartido, directorio_proyecto, sep = "\\")
    crearDirectorioNormalizado(ruta_directorio_proyecto)
    ruta_directorio_scripts <-
      paste(ruta_directorio_proyecto, "scripts", sep = "\\")
    crearDirectorioNormalizado(ruta_directorio_scripts)
    ruta_directorio_data <-
      paste(ruta_directorio_proyecto, "data", sep = "\\")
    crearDirectorioNormalizado(ruta_directorio_data)
    archivos_a_copiar_en_scripts <- c(
      "scripts/herramientasBalancesEstadosFinancieros.R",
      "scripts/usoCreacionBalancesEstadosFinancieros.R")
    scripts <- file.copy(
      from = archivos_a_copiar_en_scripts,
      to = ruta_directorio_scripts,
      overwrite = TRUE)
    data_Catalogos <- file.copy(
      from = "data/Catalogos",
      to = ruta_directorio_data,
      recursive = TRUE,
      overwrite = TRUE)
    directorios_a_copiar_en_proyecto <-
      c("html", "installers", grep("\\.Rproj$",list.files(), value = TRUE))
    varios <- file.copy(
      from = directorios_a_copiar_en_proyecto,
      to = ruta_directorio_proyecto,
      recursive = TRUE,
      overwrite = TRUE)
    return(all(c(scripts, data_Catalogos, varios)))
  }
  ruta_directorio_compartido <- "\\\\192.168.10.244\\inteligencia"
  if ( dir.exists(ruta_directorio_compartido) ) {
    copia <- copiarProyecto(ruta_directorio_compartido)
  }
  return(copia)
}

formatoTiempoHMS <- function(tiempo) {
  tiempo <- as.numeric(tiempo)
  tiempo <- as.POSIXct(tiempo, origin = "1970-01-01", tz = "UTC")
  tiempo <- format(tiempo,"%H:%M:%S")
  return(tiempo)
}

barraProgreso <- function(conjunto) {
  barra_progreso <- txtProgressBar(min = 0, max = length(conjunto), style = 3)
  numero_elementos <- length(conjunto)
  if ( exists("contador_progreso") ) {
    setTxtProgressBar(barra_progreso, contador_progreso)
    marcador_progreso_cronometro <- Sys.time()
    tiempo_transcurrido <-
      difftime(
        marcador_progreso_cronometro,
        marcador_inicio_cronometro,
        units = "sec")
    estimador_tiempo_proceso <-
      numero_elementos*(tiempo_transcurrido/(contador_progreso))
    cat("\n  \033[34mTiempo transcurrido:\033[0m",
        "\033[1;34m",formatoTiempoHMS(tiempo_transcurrido),"\033[0m", " de ",
        "\033[1;34m", formatoTiempoHMS(estimador_tiempo_proceso),"\033[0m")
    contador_progreso <<- contador_progreso + 1
  } else {
    marcador_inicio_cronometro <<- Sys.time()
    contador_progreso <<- 1
    setTxtProgressBar(barra_progreso, contador_progreso)
  }
  cat(paste0("\n[",contador_progreso,"] "))
  if ( contador_progreso == length(conjunto) ) {
    close(barra_progreso)
    rm(contador_progreso, envir = .GlobalEnv)
    rm(marcador_inicio_cronometro, envir = .GlobalEnv)
  }
}

barraProgresoReinicio <- function() {
  if (exists("contador_progreso"))
    rm(contador_progreso, envir = .GlobalEnv)
  if (exists("marcador_inicio_cronometro"))
    rm(marcador_inicio_cronometro, envir = .GlobalEnv)
}

descomprimirArchivosDirectorioZip <- function(origen, destino) {
  
  # EJEMPLO:
  # origen <- "data/Descargas/SEPS/Bases de Datos"
  # destino <- "data/Fuente/SEPS/Bases de Datos"
  # descomprimirArchivosDirectorioZip(origen, destino)
  
  requerirPaquetes("utils")
  descompresionZip <- function(ruta_origen, directorio_destino) {
    #if ( !dir.exists(directorio_destino) ) 
    crearDirectorio(directorio_destino)
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
      #if ( !dir.exists(directorio_destino_temporal) )
      crearDirectorio(directorio_destino_temporal)
      descompresionZip(ruta_origen, directorio_destino_temporal)
      ruta_comprimido_temporal <-
        file.path(directorio_destino_temporal, archivos_contenidos)
      directorio_origen_temporal <- directorio_destino_temporal
      descomprimirArchivosDirectorioZip(origen = directorio_origen_temporal,
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
      cat("\nCopiando el archivo: [", normalizePath(ruta_origen),"] ...\n")
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
  
  archivos_origen <- list.files(origen, recursive = TRUE)
  # Elegimos únicamente los archivos con extensión zip
  archivos <- grep("\\.zip$", archivos_origen, value = TRUE)
  for ( archivo in archivos ) {
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
    #if ( !dir.exists(directorio_destino) )
    crearDirectorio(directorio_destino)
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
      cat("\033[1;32mDescomprimiendo el archivo:\033[0m [", normalizePath(ruta_origen), "]\n")
    }
  }
  cat("\n")
}

eliminarArchivosZipEnDirectorio <- function(ruta_directorio) {
  # EJEMPLO:
  # ruta_directorio <- "data/Fuente/SB/Boletines Financieros Mensuales"
  # eliminarArchivosZipEnDirectorio(ruta_directorio)
  rutas_archivos <-
    list.files(ruta_directorio, recursive = TRUE, full.names = TRUE)
  rutas_zip_eliminar <- grep("\\.zip$", rutas_archivos, value = TRUE)
  unlink(rutas_zip_eliminar, recursive = TRUE)
}

depurarDirectorioPorAnio <- function(directorio_principal) {
  
  # EJEMPLO:
  # directorio_principal <- "data/Fuente/SB/Boletines Financieros Mensuales"
  # depurarDirectorioPorAnio(directorio_principal)
  
  rutas_archivos <-
    list.files(directorio_principal, recursive = TRUE, full.names = TRUE)
  for ( ruta_archivo in rutas_archivos ) {
    tiene_extension_zip <- grepl("\\.zip$", basename(ruta_archivo))
    if ( tiene_extension_zip ) unlink(ruta_archivo, recursive = TRUE)
    
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

eliminarTildes <- function(vector_texto) {
  requerirPaquetes("stringr")
  cambios <- c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
               "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U")
  texto_corregido <- stringr::str_replace_all(vector_texto, cambios)
  return(texto_corregido)
}

remplazarConTildes <- function(vector_texto) {
  requerirPaquetes("stringr")
  cambios <- c("a" = "á", "e" = "é", "i" = "í", "o" = "ó", "u" = "ú",
               "A" = "Á", "E" = "É", "I" = "Í", "O" = "Ó", "U" = "Ú")
  texto_corregido <- stringr::str_replace_all(vector_texto, cambios)
  return(texto_corregido)
}

analisisCaracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
  
  # Permite identificar en un vector de texto los caracteres ajenos a la
  # escritura en español, y retornar una tabla de los reconocimientos más
  # plausibles.
  
  # Por pruebas individuales sobre las bases de datos para cada año, se sugiere
  # que esta función se aplique preferentemente sobre la totalidad de años
  # con el fin de encontrar mejores similitudes, y evitar malas asignaciones.
  
  requerirPaquetes("stats","stringdist")
  
  vectorTexto2palabras <- function(vector_texto, separadores, expresion_regular) {
    texto_unico <- unique(vector_texto)
    texto_separado <- unlist(strsplit(texto_unico, separadores))
    palabras <- sort(unique(texto_separado))
    vector_palabras <- grep(expresion_regular, palabras, value = TRUE)
    return(vector_palabras)
  }
  palabrasCorrectas <- function(vector_texto) {
    expresion_regular <- "^[a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9]+$"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  palabrasIncorrectas <- function(vector_texto, patron_incorrecto = NULL) {
    expresion_regular <-
      if ( is.null(patron_incorrecto) ) {
        "[^a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9]"
      } else {
        patron_incorrecto
      }
    #separadores <- "[ .;/-]"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  caracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
    expresion_regular <- "[^a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9 .,;/\\(\\)\"–-]+"
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
    expresion_regular <- "^[a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9 .,;/\\(\\)\"–-]+$"
    texto_correcto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_correcto)
  }
  textoConCaracteresIncorrectos <- function(vector_texto) {
    texto_unico <- sort(unique(vector_texto))
    expresion_regular <- "[^a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9 .,;/\\(\\)\"–-]"
    texto_incorrecto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_incorrecto)
  }
  
  cat("\n\n\033[1;32mAnalizando caracteres extraños...\033[0m\n")
  barraProgresoReinicio()
  
  fraces_unicas <- sort(unique(vector_texto))
  palabras_correctas <- palabrasCorrectas(fraces_unicas)
  caracteres_incorrectos <- caracteresIncorrectos(fraces_unicas, certidumbre)
  caracter <- data.frame()
  lista_palabras <- list()
  lista_distancias <- list()
  for (k in seq_along(caracteres_incorrectos)) {
    # barraProgreso(seq_along(caracteres_incorrectos))
    # cat("Procesando caracteres extraños...\n")
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
  
  requerirPaquetes("parallel","stats","stringr")
  
  #barraProgresoReinicio()
  
  analisis_caracteres <- analisisCaracteresIncorrectos(vector_texto)$caracter
  cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
  print(analisis_caracteres)
  caracter_incorrecto <- c("  ", analisis_caracteres$original)
  caracter_correcto <- c(" ", analisis_caracteres$identificado)
  correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
  bloques_texto <-
    split(vector_texto, seq_along(vector_texto) %% parallel::detectCores())
  barraProgresoReinicio()
  texto_corregido_parelizado <-
    parallel::mclapply(
      bloques_texto,
      function(bloque) {
        barraProgreso(bloques_texto)
        cat("\033[1;32mCorreción de caracteres paralelizada...\033[0m\n")
        stringr::str_replace_all(bloque, correcciones)
      })
  texto_corregido <- unlist(texto_corregido_parelizado)
  attr(texto_corregido, "names") <- NULL
  return(texto_corregido)
} # CON ERRORES

correcionCaracteresVectorizada <- function(vector_texto) {
  
  # En pruebas con system.time, correcionCaracteresVectorizada mostró ser algo
  # mas rápida que correcionCaracteresParalelizada, sin el problema de de mover
  # los valores de la columna CUENTA
  
  requerirPaquetes("parallel","stats","stringr")
  
  analisis_caracteres <- analisisCaracteresIncorrectos(vector_texto)$caracter
  # analisis_caracteres$identificado <-
  #   remplazarConTildes(analisis_caracteres$identificado)
  cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
  print(analisis_caracteres)
  caracter_incorrecto <- c("  ", analisis_caracteres$original)
  caracter_correcto <- c(" ", analisis_caracteres$identificado)
  correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
  cat("\n\033[1;32mCorreción de caracteres vectorizada...\033[0m\n")
  texto_corregido <- stringr::str_replace_all(vector_texto, correcciones)
  return(texto_corregido)
}

# Descarga----

analisisVinculosPaginaWebSEPS <- function() {
  
  requerirPaquetes("dplyr","rvest")
  
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
  
  cat("\n\033[1mEnlaces de descarga encontrados en la página:\033[0m [",link,"]\n")
  print(links_BaseDatos)
  
  return(links_BaseDatos)
}

obtenerEnlacesDescarga <- function(enlaces_descarga, identificador) {
  
  requerirPaquetes("httr","dplyr","tools")
  
  enlaces <- enlaces_descarga
  
  barraProgresoReinicio()
  
  informacion <- data.frame()
  
  for (enlace in enlaces) {
    indice <- match(enlace, enlaces)
    
    response <- httr::HEAD(enlace)
    
    filename_temporal <-
      response$headers$`content-disposition` %>%
      sub(".*filename=\"([^\"]+)\".*", "\\1", .)
    
    nueva_fila <-
      data.frame(
        time = Sys.time(),
        link = enlace,
        url = response$url,
        filename =
          ifelse( length(filename_temporal) > 0,
                 filename_temporal, basename(enlace)),
        status_code = response$status_code,
        content_type = response$headers$`content-type`,
        last_modified =
          ifelse(!is.null(response$headers$`last-modified`),
                 response$headers$`last-modified`, NA),
        content_length =
          round(as.numeric(response$headers$`content-length`) / 2^20, 2)
      )
    
    informacion <-
      informacion %>%
      dplyr::bind_rows(nueva_fila) #%>%
      # dplyr::filter(!is.na(filename)) %>%
      # dplyr::distinct(filename, status_code, content_length, .keep_all = TRUE)
    
    if ( indice == 1 ) { cat("\nRutas de descarga:") }
    barraProgreso(enlaces)
    cat("\033[1;32mObteniendo ruta de descarga...\033[0m\n")
    cat("Del vínculo:\n\t[", enlace, "]",
        "\nse ha capturado la ruta de descarga:\n\t[", response$url, "].\n")
  }
  
  cat("\n\n\033[1mResumen:\033[0m\n")
  print(informacion)
  
  exportarReporteTabla(
    dataFrame =  informacion,
    nombre_archivo = paste("Reporte Enlaces de Descarga", identificador))
  
  #actualizarReporte(dataFrame = informacion, nombre_archivo = paste("Reporte Enlaces de Descarga", identificador))
  
  return(informacion)
}

descargarArchivosEnlacesAnalizados <- function(enlaces, informacion, ruta_destino) {
  
  desc_dir <- file.path(ruta_destino)
  crearDirectorio(desc_dir)
  url <- informacion$url
  status <- informacion$status_code
  barraProgresoReinicio()
  cat("\n\n")
  for ( k in seq_along(url) ) {
    archivo_destino <- file.path(desc_dir, basename(url[k]))
    if ( (!file.exists(archivo_destino) || grepl(format(Sys.Date(), "%Y"),url[k]) ) && status[k] == 200 ) {
      # El argumento `timeout = 300` indica que R esperará hasta 120 segundos antes de cancelar la descarga si no recibe una respuesta del servidor.
      download.file(url[k], archivo_destino, timeout = 300)
    } else if ( file.exists(archivo_destino) ) {
      cat("\nEl archivo: [", basename(url[k]),"]",
          "ya existe en el directorio: [", archivo_destino, "].\n")
    } else if ( status[k] == 404 ) {
      cat("\nEl archivo: [", basename(url[k]),"]"
          ,"NO está disponible en la dirección: [", url[k], "].\n")
    }
    barraProgreso(seq_along(url))
    cat("\033[1;32mAnalizando descarga:\033[0m ")
  }
  cat("\n\n")
}

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
        tamanio_archivo =
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
      # informacion_enlaces <-
      #   data.frame(enlaces_descarga = nodos_descarga_a %>% html_attr("href")) %>%
      #   mutate(nombre_archivo = basename(enlaces_descarga))
      enlaces_descarga <- nodos_descarga_a %>% html_attr("href")
      informacion_enlaces <-
        obtenerEnlacesDescarga(enlaces_descarga,"SB") %>%
        rename(
          fecha_acceso = "time",
          enlace_descarga = "url",
          nombre_archivo = "filename",
          fecha_modificacion = "last_modified",
          tamanio_archivo = "content_length"
        )
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
    nombre_archivo =
      paste("Reporte Enlaces de Descarga SB",
            tools::file_path_sans_ext(basename(ruta_archivo_html))))
  
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

ejecutarDescargaBoletinesFinancierosSB <- function() {
  ruta_directorio_html_SB <- "html/SB/Boletines Financieros Mensuales"
  rutas_archivos_html_SB <-
    list.files(ruta_directorio_html_SB, recursive = TRUE, full.names = TRUE)
  
  ruta_directorio_html_SB_Privados <-
    paste0(ruta_directorio_html_SB,"/Bancos Privados/",
           format(Sys.Date(),"%Y"),".html")
  ruta_directorio_html_SB_Publicas <-
    paste0(ruta_directorio_html_SB,"/Instituciones Publicas/",
           format(Sys.Date(),"%Y"),".html")
  prueba_ruta_anio_actual <-
    all(c(ruta_directorio_html_SB_Privados,
      ruta_directorio_html_SB_Publicas) %in% rutas_archivos_html_SB)
  if ( ! prueba_ruta_anio_actual  ) {
    file.create(ruta_directorio_html_SB_Privados,
                ruta_directorio_html_SB_Publicas)
  }
    
  barraProgresoReinicio()
  for (ruta in rutas_archivos_html_SB) {
    descargarBoletinesFinancierosSB(ruta)
    barraProgreso(rutas_archivos_html_SB)
    cat("\033[1;32mAnalizando archivo html en la ruta:\033[0m [", ruta, "]\n\n")
  }
}

# SEPS----

gestorDescargasDescompresionSEPS <- function() {
  
  enlaces_SEPS <- analisisVinculosPaginaWebSEPS()
  
  info_enlaces_SEPS <-
    obtenerEnlacesDescarga(enlaces_descarga = enlaces_SEPS, identificador = "SEPS")
  
  ruta_descargas_SEPS <- "data/Descargas/SEPS/Bases de Datos/Estados Financieros"
  descargarArchivosEnlacesAnalizados(enlaces_SEPS, info_enlaces_SEPS, ruta_descargas_SEPS)
  
  ruta_fuentes_SEPS <- "data/Fuente/SEPS/Bases de Datos/Estados Financieros"
  descomprimirArchivosDirectorioZip(ruta_descargas_SEPS, ruta_fuentes_SEPS) # Verificado en prueba 2023/06/09
  
}

modificarNombreColumnaSEPS <- function(tabla, nombre_nuevo, ...) {
  
  # nombre_nuevo: debe ser un string con el nombre deseado para homogeneizar
  
  requerirPaquetes("dplyr","rlang","stringr")
  
  # Lista de palabras a buscar por columna
  palabras <- list(...)
  # Buscamos las palabras clave en los nombres de las variables
  coincidencias <- lapply(palabras, function(x) stringr::str_detect(names(tabla), stringr::regex(x, ignore_case = TRUE)))
  # Determinamos el nombre de columna a cambiar
  nombre_anterior <- names(tabla)[Reduce(`&`, coincidencias)]
  # Se puede pasar el nuevo nombre de la columna como una variable desde fuera de la función dplyr::rename(). Para hacerlo, puedes usar la función rlang::sym() para convertir el valor de la variable en un símbolo y luego usar el operador !! para evaluarlo dentro de dplyr::rename()
  tabla <- dplyr::rename(tabla, !!rlang::sym(nombre_nuevo) := nombre_anterior)
  # Resultado de la función
  return(tabla)
}

generarListaTablasSEPS <- function() {
  requerirPaquetes("dplyr","readr")
  directorio_principal <- "data/Fuente/SEPS/Bases de Datos"
  archivos <- list.files(path = directorio_principal, full.names = TRUE, recursive = TRUE)
  lista_tablas_SEPS <- list()
  for ( archivo in archivos ) {
    barraProgreso(archivos)
    cat("\033[1;32mImpotando y procesando el archivo:\033[0m",
        "[", normalizePath(archivo), "]\n")
    nombre_tabla <- basename(dirname(archivo))
    lista_tablas_SEPS[[nombre_tabla]] <-
      archivo %>%
      readr::read_delim(., guess_max = 1000 ) %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "FECHA", "fecha", "corte") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "SEGMENTO", "segmento") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "RUC", "ruc") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "RAZON_SOCIAL", "razon", "social") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "DESCRIPCION", "descripcion", "cuenta") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "CODIGO", "cuenta") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "CUENTA", "descripcion") %>%
      modificarNombreColumnaSEPS(., nombre_nuevo = "VALOR", "saldo") %>%
      dplyr::mutate(
        `FECHA` = lubridate::ymd(`FECHA`),
        `SEGMENTO` = as.character(`SEGMENTO`),
        `RUC` = as.character(`RUC`),
        `RAZON_SOCIAL` = as.character(`RAZON_SOCIAL`),
        `CUENTA` = as.character(`CUENTA`),
        `CODIGO` = as.integer(`CODIGO`),
        `VALOR` = as.numeric(gsub(",", ".", `VALOR`)),
        `SEGMENTO` = ifelse(`SEGMENTO` == "SEGMENTO 1 MUTUALISTA","MUTUALISTA", `SEGMENTO`)
      ) %>%
      # NO SE ENCONTRÓ
      dplyr::mutate(
        `RAZON_SOCIAL` = chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", `RAZON_SOCIAL`)
      )
  }
  return(lista_tablas_SEPS)
}

crearEstadosFinancierosSEPS <- function() {

  requerirPaquetes("data.table","dplyr")
  tic_general <- Sys.time()
  
  gestorDescargasDescompresionSEPS() # Verificado en prueba 2023/06/22
  
  lista_SEPS <- generarListaTablasSEPS()
  
  tabla_concatenada <- # Verificado en prueba 2023/06/22
    lista_SEPS %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::rename(
      `RAZON_SOCIAL_ORIGINAL` = `RAZON_SOCIAL`,
      `CUENTA_ORIGINAL` = `CUENTA`) %>% 
    dplyr::mutate(
      `RAZON_SOCIAL_CORREGIDA` = correcionCaracteresVectorizada(`RAZON_SOCIAL_ORIGINAL`),
      `CUENTA_CORREGIDA` = correcionCaracteresVectorizada(`CUENTA_ORIGINAL`)) %>%
    dplyr::mutate(
      `CORRECCION` =
        (`RAZON_SOCIAL_ORIGINAL` != `RAZON_SOCIAL_CORREGIDA`) | 
        (`CUENTA_ORIGINAL` != `CUENTA_CORREGIDA`))
  
  exportarResultadosCSV(tabla_concatenada,"SEPS Estados Financieros")
  
  cat("\n\n  \033[1;34mDuración total del proceso \"Estados Financieros SEPS\":",
      formatoTiempoHMS(difftime(Sys.time(), tic_general, units = "secs")), "\033[0m\n")
  
  return(tabla_concatenada)
}

exportarEstadosFinancierosSEPSmensualSIEVA <- function(data_frame) {
  requerirPaquetes("dplyr","openxlsx")
  
  ultima_fecha <- max(SEPS$FECHA)
  
  SEPS_SIEVA_mensual <-
    SEPS %>%
    filter(
      FECHA == ultima_fecha,
      SEGMENTO %in% c("MUTUALISTA", "SEGMENTO 1", "SEGMENTO 2", "SEGMENTO 3")) %>%
    mutate(FECHA = format(FECHA, "%d/%m/%Y")) %>%
    select(SEGMENTO, CODIGO, VALOR, FECHA, RUC) %>%
    distinct() %>%
    rename(
      segmento = SEGMENTO,
      cuenta = CODIGO,
      valor = VALOR,
      `fecha (dd/mm/AAAA)` = FECHA,
      ruc = RUC
    )
  
  ruta_dir_compartida <- "\\\\192.168.10.244\\inteligencia\\SIEVA SEPS Mensual"
  nombre_archivo <- paste0("Balance SIEVA SEPS ", ultima_fecha, ".xlsx")
  ruta_archivo <- file.path(ruta_dir_compartida, nombre_archivo)
  openxlsx::write.xlsx(SEPS_SIEVA_mensual, ruta_archivo)
}

# CUNCLUIDO----

verificarInstalacion <- function(ruta_intalacion) {
  
  # Esta función ejecuta un comando de PowerShell para buscar la ubicación del ejecutable de Excel en el Registro de Windows
  
  # EJEMPLO:
  # ruta_intalacion <- "'HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe'"
  # verificarInstalacion(ruta_intalacion)
  # verificarInstalacion("'C:\\Program Files\\7-Zip\\7zFM.exe'")
  
  # Script de Power Shell para verificar instalción en determinada ruta
  script <- paste0("Test-Path ", ruta_intalacion)
  # Ejecutar el script de PowerShell y capturar el resultado
  is_installed <- system2("powershell", script, stdout = TRUE)
  
  return(as.logical(is_installed))
}

verificarInstalacionWindows <- function(nombre_aplicacion) {
  
  # Verifica si una aplicación de nombre similar está instaldo en el sistema
  
  # EJEMPLOS:
  # nombre_aplicacion <- "7-Zip"
  # nombre_aplicacion <- "Microsoft 365"
  # verificarInstalacionWindows(nombre_aplicacion)
  # verificarInstalacionWindows("Zip")
  # verificarInstalacionWindows("Office")
  # verificarInstalacionWindows("Microsoft 365")
  
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

executarInstalardor7zip <- function() {
  
  # Verifica si 7zip está instalado y si no lo está, ejecuta el instalador la aplicación almacenado en el directorio del proyecto
  
  # EJEMPLO: executarInstalardor7zip()
  
  # Define el nombre del archivo de instalación
  ruta_7zip <- "installers/7z2201-x64.exe"
  # Define la ruta del archivo de instalación
  file_path <- normalizePath(ruta_7zip)
  # Verifica si 7-Zip está instalado
  #prueba_instalacion <- verificarInstalacion("'C:\\Program Files\\7-Zip\\7zFM.exe'")
  prueba_instalacion <- verificarInstalacionWindows("7-Zip")
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
  prueba_instalacion_7zip <- verificarInstalacionWindows("7-Zip")
  
  if ( prueba_instalacion_7zip == TRUE ) {
    # Realizamos la descompresión
    procedimiento_descompresion()
  } else {
    # Instalamos 7-Zip
    executarInstalardor7zip()
    # Realizamos la descompresión
    procedimiento_descompresion()
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
  
  requerirPaquetes("tools")
  
  verificadorExcel <- function() {
    # Ruta genérica para que PowerShell encuentre a Excel
    ruta_excel <- "'HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe'"
    return ( verificarInstalacion(ruta_excel) )
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

frecuenciaEmpiricaRelativaOcurrenciaDecimalEnTexto <- function(cadena_texto) {
  
  # Esta función calcula la frecuencia relativa de la ocurrencia de la expresión de un número decimal en una columna.
  
  # cadena_texto: es un vector tipo char que puede contener diferentes tipos de información todos convertidos en carácteres.
  
  # Ejemplo: frecuenciaEmpiricaRelativaOcurrenciaDecimalEnTexto(c("a",1,3.14,Sys.Date())) devolverá 0.25 como la frecuencia relativa por le 3.14
  
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
        frecuenciaEmpiricaRelativaOcurrenciaDecimalEnTexto(cadena_texto)
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
  
  requerirPaquetes("lubridate","parsedate","stringdist")
  
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

analisisTextoSimilar <- function(texto, catalogo, precision) {
  distancia <-
    stringdist::stringsimmatrix(catalogo, texto, method = "cosine")
    #stringdist::stringsimmatrix(catalogo, texto, method = "jw")
    # "cosine" parece funcionar mejor para comparar fraces
  rownames(distancia) <- catalogo
  colnames(distancia) <- texto
  incidice_maximal <- apply(distancia, 2, which.max)
  texto_similar <- catalogo[incidice_maximal]
  distancia_maxima <-
    sapply(seq_along(incidice_maximal),
           function(k) distancia[incidice_maximal[k],
                                 names(incidice_maximal[k])])
  identificacion <-
    data.frame(
      "Original" = texto,
      "Similar" = texto_similar,
      "DistanciaJW" = distancia_maxima) %>%
    dplyr::mutate(Correccion = ifelse(DistanciaJW < precision, NA, Similar))
  return(identificacion)
}

modificarNombreColumnaSB <- function(tabla, precision = NULL, catalogo = NULL) {
  
  # Función para identificar, modificar los nombres de las columnas de una tabla utilizando un catálogo de operadores y el método de similitud de cadenas Jaro-Winkler.
  
  # Ejemplo de uso: tabla <- modificarNombreColumnaSB(tabla = tabla)
  
  if ( is.null(precision) ) precision = 0.85
  if ( is.null(catalogo) ) {
    requerirPaquetes("readxl")
    catalogo <- readxl::read_excel("data/Catalogos/Catalogo Operadores.xlsx")
    catalogo_complementario <- data.frame(
      RUC = rep(NA),
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
  nombres_columnas <- colnames(tabla)
  nombres_catalogo <- catalogo$Operadora
  analisis_nombres_columnas <-
    analisisTextoSimilar(nombres_columnas, nombres_catalogo, precision)
  nombres_sin_repeticion <- make.unique(analisis_nombres_columnas$Correccion)
  analisis_nombres_columnas$Correccion <- nombres_sin_repeticion
  indice_fila_cambios <-
    which(
      analisis_nombres_columnas$Original ==
        analisis_nombres_columnas$Correccion)
  detalle_cambios <-
    analisis_nombres_columnas[-indice_fila_cambios,c("Original","Correccion")]
  rownames(detalle_cambios) <- NULL
  if ( nrow(detalle_cambios) > 0 ) {
    cat("\n\033[1mNombres de columnas modificados:\033[0m\n")
    print(detalle_cambios)
  }
  names(tabla) <- nombres_sin_repeticion
  
  return(tabla)
}

nombreHojaSimilar <- function(ruta_libro, nombre_hoja_buscado) {
  
  # Determina la hoja en un libro de excel que tiene mayor similitud a la hoja buscada
  
  requerirPaquetes("readxl","stringdist")
  
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
  
  requerirPaquetes("dplyr","readxl")
  
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
  tabla_modificada <-
    tabla %>%
    # Eliminamos las columnas que no contengan caracteres alfabéticos
    select( -matches("^[^[:alpha:]]+$", .) ) %>%
    # Eliminamos las filas que contienen únicamente valores NA
    filter( !if_all(everything(), is.na) ) %>%
    # Empleamos la función creada para modificar los nombres de las columnas según un catálogo por defecto
    modificarNombreColumnaSB(tabla = ., precision = 0.8) %>%
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
  
  return(tabla_modificada)
}

compilarHojasBalanceFinancieroSB <- function(ruta_directorio = NULL) {
  
  # Esta función realiza todo el proceso necesario para crear la base de datos de los Balances Financieros mensuales de la SB
  
  requerirPaquetes("dplyr","purrr","readxl","reshape2","tools")
  
  # # Cerramos todos los libros de Excel abiertos
  # system2("powershell", "Get-Process excel | Foreach-Object { $_.CloseMainWindow() }")
  # Establecemos la ruta del directorio fuente de los libros de Excel con los "Boletines Financieros mensuales"
  if ( is.null(ruta_directorio) ) {
    ruta_directorio <-
      "data/Fuente/SB/Boletines Financieros Mensuales/Bancos Privados"
      #"data/Fuente/SB/Boletines Financieros Mensuales/Instituciones Publicas"
  }
  # Determinamos los archivos presentes en directorio fuente
  archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
  # Descartamos los archivos con extensión zip
  #tiene_extension_zip <- tools::file_ext(archivos_directorio) == "zip"
  tiene_extension_zip <- grepl("\\.zip$", archivos_directorio)
  archivos_directorio <- archivos_directorio[!tiene_extension_zip]
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
  # prueba_anio <- grepl("(201[3-9])|(202[0-9])",rutas_libros)
  anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
  expresion_regular_anios <- paste(seq(2013,anio_actual), collapse = "|")
  prueba_anio <- grepl(expresion_regular_anios,rutas_libros)
  # Filtramos las rutas con los años establecidos
  rutas_libros_seleccionados <- rutas_libros[prueba_anio]
  # Cerramos todos los libros seleccionados para que no generen ningún error al procesarlos
  cat("\n\nCerrando los los libros de Excel realacionados...\n")
  cerrarLibroExcel(rutas_libros_seleccionados)
  # Limpiamos la barra de progreso
  barraProgresoReinicio()
  # Inicializamos la lista de las tablas concatenadas de BALANCE y PYG
  lista_tablas_BAL_PYG_concatenadas <- list()
  # Definimos el bucle de ejecución
  for ( ruta_libro in rutas_libros_seleccionados ) {
    # Importamos las 20 primeras filas de la hoja BALANCE para identificar la fecha de corte
    hoja <-
      suppressMessages(
        readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
    # Identificamos la fecha de corte
    fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
    # Extraemos la tabla de BALANCE
    tabla_BAL <-
      hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte)
    # Extraemos la tabla de PYG
    tabla_PYG <-
      hojaToTablaBoletinesFinancierosSB(ruta_libro, "PYG", fecha_corte)
    # Definimos el nombre de para cada tabla
    nombre_tabla <- basename(ruta_libro)
    # Asignamos la tabla concatenada de BALANCE y PYG a un elemento de la lista de tablas
    lista_tablas_BAL_PYG_concatenadas[[nombre_tabla]] <-
      dplyr::bind_rows(tabla_BAL,tabla_PYG)
    # Ejecutamos el código para la barra de progreso
    barraProgreso(rutas_libros_seleccionados)
    # Mostramos la ruta del archivo en proceso
    cat("\033[1;32mImportando y procesando el archivo:\033[0m",
        "[", normalizePath(ruta_libro), "]\n")
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
  exportarReporteTabla(
    reporte_consolidacion_BAL_PYG,
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

agregarRUCenSB <- function(tabla, ruta_catalogo = NULL) {

  requerirPaquetes("dplyr","readxl","stringr")
  if ( is.null(ruta_catalogo) ) {
    ruta_catalogo <- "data/Catalogos/Catalogo Operadores.xlsx"
  }
  precision <- 0.9
  catalogo <- readxl::read_excel(ruta_catalogo)
  RAZON_SOCIAL_SB <- sort(unique(tabla$RAZON_SOCIAL))
  RAZON_SOCIAL_catalogo <- catalogo$Operadora
  distancia <-
    stringdist::stringsimmatrix(
      catalogo$Operadora, RAZON_SOCIAL_SB, method = "jw")
  rownames(distancia) <- RAZON_SOCIAL_catalogo
  colnames(distancia) <- RAZON_SOCIAL_SB
  incidice_nombre_maximal <- apply(distancia, 2, which.max)
  RUC_identificado <- catalogo$RUC[incidice_nombre_maximal]
  distancia_maxima <-
    sapply(seq_along(incidice_nombre_maximal),
           function(k) distancia[incidice_nombre_maximal[k],
                                 names(incidice_nombre_maximal[k])])
  identificacion_RUC <-
    data.frame(
      "NombreEnSB" = RAZON_SOCIAL_SB,
      "NombreEnCatalogo" = RAZON_SOCIAL_catalogo[incidice_nombre_maximal],
      "RUC" = RUC_identificado,
      "DistanciaJW" = distancia_maxima) %>%
    dplyr::mutate(RUC = ifelse(DistanciaJW < precision, NA, RUC))
  indice_correspondencia <- match(tabla$RAZON_SOCIAL, identificacion_RUC$NombreEnSB)
  tabla$RUC <- identificacion_RUC$RUC[indice_correspondencia]
  return(tabla)
}

crearBalancesFinancierosSB <- function() {
  
  # Esta función junta las tablas consolidadas de Balances Financieros tanto de la Banca pública como privada
  
  requerirPaquetes("dplyr")
  
  tic_general <- Sys.time()
  
  # ETAPA 0: Descargar los "Boletines Financieros Mensuales" del portal de la SB ----
  ejecutarDescargaBoletinesFinancierosSB() # Verificado en prueba 2023/06/09, crear una alternativa recursiva para error de descarga
  
  # ETAPA 1: Descompresión de los archivos descargados del portal de la SB ----
  origen <- "data/Descargas/SB/Boletines Financieros Mensuales"
  destino <- "data/Fuente/SB/Boletines Financieros Mensuales"
  descomprimirArchivosDirectorioZip(origen, destino) # Verificado en prueba 2023/06/09
  #eliminarArchivosZipEnDirectorio(destino)
  #depurarDirectorioPorAnio(destino)
  
  # ETAPA 2: Consolidación del la tabla de Balance Financiero de la Banca privada ----
  ruta_directorio_privada = "data/Fuente/SB/Boletines Financieros Mensuales/Bancos Privados"
  privada <- compilarHojasBalanceFinancieroSB(ruta_directorio_privada) %>%
    mutate(SEGMENTO = "PRIVADA") # Verificado en prueba individual 2023/05/11
  
  # ETAPA 3: Consolidación del la tabla de Balance Financiero de la Banca privada ----
  ruta_directorio_publica = "data/Fuente/SB/Boletines Financieros Mensuales/Instituciones Publicas"
  publica <- compilarHojasBalanceFinancieroSB(ruta_directorio_publica) %>%
    mutate(SEGMENTO = "PUBLICA")
  
  # ETAPA 4: Concatenación todas de tablas consolidadas ----
  cat("\n\nConcatenando tablas y agregando RUC...\n")
  consolidada <-
    dplyr::bind_rows(privada,publica) %>%
    agregarRUCenSB() %>%
    dplyr::select(FECHA, SEGMENTO, RUC, RAZON_SOCIAL, CODIGO, CUENTA, VALOR)
  
  # ETAPA 5: Exportación de base de datos generada ----
  exportarResultadosCSV(consolidada,"SB Balances Financieros")
  cat("\n\n  \033[1;34mDuración total del proceso \"Balances Financieros SB\":",
      formatoTiempoHMS(difftime(Sys.time(), tic_general, units = "secs")), "\033[0m\n")
  
  return(consolidada)
}

fusionarBalancesSBEstadosSEPSFinancieros <- function() {
  
  categorias_de_configuracion <-
    c("LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_NUMERIC", "LC_TIME")
  configuracion_local_proceso_R <-
    sapply(categorias_de_configuracion, Sys.getlocale)
  configuracion_BalancesEstadosFinancierosSFN <-
    c(rep("Spanish_Ecuador.utf8",3),"C","Spanish_Ecuador.utf8")
  mapply(Sys.setlocale,
    categorias_de_configuracion, configuracion_BalancesEstadosFinancierosSFN)
  num_max_impresiones_consola <- getOption("max.print")
  options(max.print = 10000)
  
  tic_general <- Sys.time()
  
  requerirPaquetes()
  
  SB <- crearBalancesFinancierosSB() # Verificado en prueba 2023/06/09
  SEPS <- crearEstadosFinancierosSEPS() # Verificado en prueba 2023/06/21
  BEF <- rbind(SEPS, SB)
  
  exportarEstadosFinancierosSEPSmensualSIEVA() # Verificado en prueba 2023/06/16
  
  exportarResultadosCSV(BEF,"Balances Estados Financieros")
  
  ruta_dir_compartida <- "\\\\192.168.10.244\\inteligencia"
  exportarResultadosCSV(BEF,"Balances Estados Financieros",ruta_dir_compartida)
  
  cat("\n\n  \033[1;34mDuración total del proceso:",
      formatoTiempoHMS(difftime(Sys.time(), tic_general, units = "secs")), "\033[0m\n\n")
  
  options(max.print = num_max_impresiones_consola)
  mapply(Sys.setlocale, 
         categorias_de_configuracion, configuracion_local_proceso_R)
  
  return(list(SB=SB,SEPS=SEPS,BEF=BEF))
}
