

hojaToTablaBoletinesFinancierosSB_v2 <- function(ruta_libro, nombre_hoja, fecha_corte = NULL) {
  
  requerirPaquetes("dplyr","readxl")

  nombre_hoja <- nombreHojaSimilar(ruta_libro, nombre_hoja)
  hoja <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = FALSE, n_max = 30))
  fecha_corte <-
    if ( is.null(fecha_corte) ) {
      # Determinamos la fecha más probable contenida en la hoja importada
      analisisDifusoNLPFechaCorte(hoja)
    } else {
      fecha_corte
    }
  indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
  nombres_columnas <- unname(unlist(hoja[indice_fila_nombres_columnas,]))
  tabla_prueba <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20))
  if ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 ) {
    indice_fila_nombres_columnas <- indice_fila_nombres_columnas - 2
    while ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 & indice_fila_nombres_columnas <= 20 ) {
      indice_fila_nombres_columnas <- indice_fila_nombres_columnas + 1
      tabla_prueba <- suppressMessages(readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20))
    }
  }
  advertencias <- NULL
  tabla <-
    withCallingHandlers(
      suppressMessages(
        readxl::read_excel(ruta_libro,
                           sheet = nombre_hoja,
                           col_names = TRUE,
                           skip = indice_fila_nombres_columnas)),
      warning = function(w) {
        advertencias <<- c(advertencias, w$message)
        invokeRestart("muffleWarning")
      }
    )
  attr(tabla, "advertencias") <- advertencias
  colnames(tabla) <- 
    tabla %>% colnames() %>% toupper() %>%
    chartr("[ÁÉÍÓÚ]", "[AEIOU]", .) %>% unique() %>%
    gsub(" {2,}","",.) %>%
    gsub(" $","",.)
  tabla_modificada <-
    tabla %>%
    select( -matches("^[^[:alpha:]]+$", .) ) %>%
    # mutate(
    #   CODIGO = as.character(CODIGO),
    #   CUENTA = as.character(CUENTA)) %>%
    # mutate_at(-c(CODIGO, CUENTA), as.numeric) %>%
    # filter( !(is.na(CODIGO) & is.na(CUENTA)) ) %>%
    # filter( !if_all(-CUENTA, is.na) ) %>%
    # filter( !(grepl("[[:alpha:]]+",CODIGO) & if_all(-CODIGO, is.na)) ) %>%
    # filter( !(grepl("[[:alpha:]]+",CODIGO) & grepl("[[:alpha:]]+",CUENTA) & if_all(-c(CODIGO,CUENTA), is.na)) ) %>%
    # mutate_at(-c(CODIGO, CUENTA), as.numeric) %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(FECHA, everything())
  
  return(tabla_modificada)
}


listarHojasBalanceFinancieroSB <- function(ruta_directorio = NULL) {
  
  requerirPaquetes("dplyr","purrr","readxl","reshape2","tools")
  
  if ( is.null(ruta_directorio) ) {
    ruta_directorio <-
      "data/Fuente/SB/Boletines Financieros Mensuales"
  }
  archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
  tiene_extension_zip <- grepl("\\.zip$", archivos_directorio)
  archivos_directorio <- archivos_directorio[!tiene_extension_zip]
  rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  rutas_transformar <- rutas_libros[tools::file_ext(rutas_libros) == "xlsb"]
  if ( length(rutas_transformar) > 0 ) {
    purrr::map(rutas_transformar, xlsb2xlsx)
    archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
    rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  }
  anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
  expresion_regular_anios <- paste(seq(2013,anio_actual), collapse = "|")
  prueba_anio <- grepl(expresion_regular_anios,rutas_libros)
  rutas_libros_seleccionados <- rutas_libros[prueba_anio]
  cat("\n\nCerrando los los libros de Excel realacionados...\n")
  cerrarLibroExcel(rutas_libros_seleccionados)
  barraProgresoReinicio()
  lista_tablas_BAL_concatenadas <- list()
  lista_tablas_PYG_concatenadas <- list()
  for ( ruta_libro in rutas_libros_seleccionados ) {
    hoja <-
      suppressMessages(
        readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
    fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
    tabla_BAL <-
      hojaToTablaBoletinesFinancierosSB_v2(ruta_libro, "BALANCE", fecha_corte)
    tabla_PYG <-
      hojaToTablaBoletinesFinancierosSB_v2(ruta_libro, "PYG", fecha_corte)
    nombre_tabla <- basename(ruta_libro)
    lista_tablas_BAL_concatenadas[[nombre_tabla]] <- tabla_BAL
    lista_tablas_PYG_concatenadas[[nombre_tabla]] <- tabla_PYG
    barraProgreso(rutas_libros_seleccionados)
    cat("\033[1;32mImportando y procesando el archivo:\033[0m",
        "[", normalizePath(ruta_libro), "]\n")
  }
  
  tabla_BAL_PYG <- dplyr::bind_rows(lista_tablas_BAL_PYG_concatenadas)
  registro_advertencias <-
    sapply(seq_along(lista_tablas_BAL_PYG_concatenadas),
           function(k) attr(lista_tablas_BAL_PYG_concatenadas[[k]],"advertencias"))
  names(registro_advertencias) <- names(lista_tablas_BAL_PYG_concatenadas)
  reporte_consolidacion_BAL_PYG <-
    data.frame(
      Archivo = names(unlist(registro_advertencias)),
      Advertencia = unname(unlist(registro_advertencias)))
  exportarReporteTabla(
    reporte_consolidacion_BAL_PYG,
    paste("Reporte Advertencias en Consolidación Balances Financieros SB",
          basename(ruta_directorio)))
  tabla_BAL_PYG_fundida <-
    reshape2::melt(tabla_BAL_PYG,
                   id.vars = colnames(tabla_BAL_PYG)[1:3],
                   variable.name = "RAZON_SOCIAL",
                   value.name = "VALOR")
  
  return(tabla_BAL_PYG_fundida)
}

# ----
lista_bancos_privados <- c(lista_tablas_BAL_concatenadas,lista_tablas_PYG_concatenadas)
indices_lista <- length(lista_bancos_privados)
nombres_bancos_privados <- sapply(seq_len(indices_lista), function(k)
  names(lista_bancos_privados[[k]])) %>% unlist() %>% unique() %>% sort()

indice_nombres_columnas_sin_sentido <- grep("^[^[:alpha:]]+$", nombres_bancos_privados)

nombres_bancos_privados[indice_nombres_columnas_sin_sentido]

bancos <- nombres_bancos_privados[-indice_nombres_columnas_sin_sentido]

bancos_depurados <- 
  bancos %>% toupper() %>%
  chartr("[ÁÉÍÓÚ]", "[AEIOU]", .) %>% unique() %>%
  #.[!grepl("CODIGO|CUENTA|FECHA", .)] %>%
  gsub(" {2,}","",.) %>%
  gsub(" $","",.)

similitud <- stringdist::stringsimmatrix(bancos_depurados,bancos_depurados, method = "cosine")
rownames(similitud) <- bancos_depurados
colnames(similitud) <- bancos_depurados


write.xlsx(data.frame(bancos_depurados),"catalogo.xlsx")

