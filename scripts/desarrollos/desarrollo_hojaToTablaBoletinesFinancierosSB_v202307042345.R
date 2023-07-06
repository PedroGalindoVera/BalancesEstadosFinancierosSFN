hojaToTablaBoletinesFinancierosSB <- function(ruta_libro, nombre_hoja, fecha_corte = NULL) {
  
  # Esta función permite extraer la tabla de datos contenida en un hoja de cálculo correspondiente a los "Boletines Financieros mensuales" de la SB
  
  # ARGUMENTOS:
  # ruta_libro <- "data/Fuente/SB/PRIVADA/2023/FINANCIERO MENSUAL BANCA PRIVADA 2023_02.xlsx"
  # nombre_hoja <- "BALANCE"
  # fecha_corte <- "2023-02-29"
  # EJEMPLO: tabla <- hojaToTablaBoletinesFinancierosSB(ruta_libro, nombre_hoja, fecha_corte)
  
  requerirPaquetes("dplyr","readxl")
  
  nombreHojaSimilar <- function(ruta_libro, nombre_hoja_buscado) {
    requerirPaquetes("readxl","stringdist")
    nombres_hojas <- readxl::excel_sheets(ruta_libro)
    similitud <- stringdist::stringsimmatrix(nombre_hoja_buscado, nombres_hojas, method = "jw")
    indice_hoja_similar <- which.max(similitud)
    nombre_hoja_similar <- nombres_hojas[indice_hoja_similar]
    return(nombre_hoja_similar)
  }
  estandarizarNombreColumna <- function(tabla) {
    nombres_columnas_estandarizados <-
      tabla %>%
      colnames() %>%
      toupper() %>%
      chartr("[ÁÉÍÓÚ]", "[AEIOU]", .) %>%
      gsub(" {2,}"," ",.) %>%
      gsub("^ | $","",.)
    colnames(tabla) <- nombres_columnas_estandarizados
    return(tabla)
  }
  estandarizarRectificarCODIGO <- function(tabla) {
    tabla_CODIGO_estandarizado <-
      tabla %>%
      filter( ! grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% # tabla %>% filter( grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% View()
      mutate( CODIGO = reemplazarTexto(c(100,300,400),c(1,2,3),CODIGO),
              CUENTA = 
                reemplazarTexto(
                  c("^ACTIVO$","^TOTAL ACTIVO$","^TOTAL ACTIVOS$","^PASIVO$","^TOTAL PASIVO$","^TOTAL PASIVOS$","^TOTAL PATRIMONIO$","^TOTAL INGRESOS$"),
                  c("ACTIVOS","ACTIVOS","ACTIVOS","PASIVOS","PASIVOS","PASIVOS","PATRIMONIO","INGRESOS"),
                  CUENTA) )
    
    catalogo_CODIGO_SB <-
      tabla_CODIGO_estandarizado %>%
      group_by(CODIGO, CUENTA) %>%
      #summarise(CANTIDAD = n()) %>% # innecesario y produce advertencia
      filter( grepl("^[0-9]+$", CODIGO) ) %>%
      mutate( CODIGO = as.integer(CODIGO) ) %>%
      filter( CODIGO > 0 )
    
    indices_completar <- match(tabla_CODIGO_estandarizado$CUENTA, catalogo_CODIGO_SB$CUENTA)
    
    tabla_CODIGO_rectificado <-
      tabla_CODIGO_estandarizado %>%
      mutate( CODIGO = ifelse(is.na(CODIGO), catalogo_CODIGO_SB$CODIGO[indices_completar], CODIGO) ) %>%
      mutate( SUMA = select(.,-CODIGO,-CUENTA) %>% rowSums(na.rm = TRUE) ) %>%
      group_by(CODIGO, CUENTA) %>%
      filter(SUMA == max(SUMA)) %>%
      select( -SUMA ) %>%
      filter( ! is.na(CODIGO) ) %>%
      distinct() %>%
      arrange(CODIGO)
    
    return(tabla_CODIGO_rectificado)
  }
  modificarTablaSB <- function(tabla, fecha_corte) {
    tabla_modificada <-
      tabla %>%
      estandarizarNombreColumna() %>%
      select( -matches("^[^[:alpha:]]+$", .) ) %>%
      filter_all( any_vars( ! is.na(.) & . != 0 ) ) %>%
      mutate(
        CODIGO = as.character(CODIGO),
        CUENTA = as.character(CUENTA)) %>%
      mutate_at( vars(-CODIGO, -CUENTA), as.numeric) %>%
      filter( ! ( is.na(CODIGO) & is.na(CUENTA) ) ) %>%
      estandarizarRectificarCODIGO() %>%
      mutate( FECHA = fecha_corte ) %>%
      select( FECHA, everything() )
    return(tabla_modificada)
  }
  
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
  tabla_modificada <- modificarTablaSB(tabla, fecha_corte)
  
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
      "data/Fuente/SB/Boletines Financieros Mensuales"
      #"data/Fuente/SB/Boletines Financieros Mensuales/Bancos Privados"
      #"data/Fuente/SB/Boletines Financieros Mensuales/Instituciones Publicas"
  }
  archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
  # Descartamos los archivos con extensión zip
  #tiene_extension_zip <- tools::file_ext(archivos_directorio) == "zip"
  tiene_extension_zip <- grepl("\\.zip$", archivos_directorio)
  archivos_directorio <- archivos_directorio[!tiene_extension_zip]
  rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  rutas_transformar <- rutas_libros[tools::file_ext(rutas_libros) == "xlsb"]
  # Realizamos los cambios solo si son necesarios
  if ( length(rutas_transformar) > 0 ) {
    purrr::map(rutas_transformar, xlsb2xlsx)
    archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
    # Volvemos a determinar todas las rutas de los archivos en el directorio luego del cambio de formato
    rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  }
  anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
  expresion_regular_anios <- paste(seq(2013,anio_actual), collapse = "|")
  prueba_anio <- grepl(expresion_regular_anios,rutas_libros)
  rutas_libros_seleccionados <- rutas_libros[prueba_anio]
  cat("\n\nCerrando los los libros de Excel realacionados...\n")
  cerrarLibroExcel(rutas_libros_seleccionados)
  
  barraProgresoReinicio()
  lista_tablas_BAL_PYG_fundidas <- list()
  lista_tablas_BAL_PYG <- list()
  # DESARROLLO - PRUEBAS
  for ( ruta_libro in rutas_libros_seleccionados ) {
    hoja <-
      suppressMessages(
        readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
    fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
    tabla_BAL <-
      hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte)
    tabla_PYG <-
      hojaToTablaBoletinesFinancierosSB(ruta_libro, "PYG", fecha_corte)
    nombre_tabla <- basename(ruta_libro)
    #
    lista_tablas_BAL_PYG[[nombre_tabla]] <-
      dplyr::bind_rows(tabla_BAL,tabla_PYG) %>% 
      arrange(CODIGO)
    #
    lista_tablas_BAL_PYG_fundidas[[nombre_tabla]] <-
      dplyr::bind_rows(tabla_BAL,tabla_PYG) %>%
      reshape2::melt(.,
                     id.vars = colnames(.)[1:3],
                     variable.name = "RAZON_SOCIAL",
                     value.name = "VALOR")
    barraProgreso(rutas_libros_seleccionados)
    cat("\033[1;32mImportando y procesando el archivo:\033[0m",
        "[", normalizePath(ruta_libro), "]\n")
  }
  
  tabla_BAL_PYG <-
    dplyr::bind_rows(lista_tablas_BAL_PYG_fundidas) %>%
    dplyr::mutate(RAZON_SOCIAL = as.character(RAZON_SOCIAL))
  
  return(tabla_BAL_PYG)
}

data_frame <- lista_tablas_BAL_PYG[[1]]

SB <-
  data_frame %>%
  filter( ! grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% # data_frame %>% filter( grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% View()
  mutate( CODIGO = reemplazarTextoParticionado(c(100,300,400),c(1,2,3),CODIGO),
          CUENTA = 
            reemplazarTextoParticionado(
              c("^ACTIVO$","^TOTAL ACTIVO$","^TOTAL ACTIVOS$","^PASIVO$","^TOTAL PASIVO$","^TOTAL PASIVOS$","^TOTAL PATRIMONIO$","^TOTAL INGRESOS$"),
              c("ACTIVOS","ACTIVOS","ACTIVOS","PASIVOS","PASIVOS","PASIVOS","PATRIMONIO","INGRESOS"),
              CUENTA) )

catalogo_CODIGO_SB <-
  SB %>%
  group_by(CODIGO, CUENTA) %>%
  summarise(CANTIDAD = n()) %>%
  filter( grepl("^[0-9]+$", CODIGO) ) %>%
  mutate( CODIGO = as.integer(CODIGO) ) %>%
  filter( CODIGO > 0 )

indices <- match(SB$CUENTA, catalogo_CODIGO_SB$CUENTA)

# SB_ <- SB %>%
#   mutate( CODIGO = ifelse(is.na(CODIGO), catalogo_CODIGO_SB$CODIGO[indices], CODIGO) ) %>%
#   mutate( CODIGO = as.integer(CODIGO) ) %>% #distinct()  %>%
#   group_by(CODIGO, CUENTA) %>%
#   filter(row_number() == n()) %>%
#   arrange(CODIGO)

SB_ <- SB %>%
  mutate( CODIGO = ifelse(is.na(CODIGO), catalogo_CODIGO_SB$CODIGO[indices], CODIGO) ) %>%
  #mutate( SUMA = select(., 3, ncol(.)) %>% rowSums(na.rm = TRUE) )
  #mutate( SUMA = .[,-c(1,2)] %>% rowSums(na.rm = TRUE) )
  mutate( SUMA = select(.,-CODIGO,-CUENTA) %>% rowSums(na.rm = TRUE) ) %>%
  group_by(CODIGO, CUENTA) %>%
  filter(SUMA == max(SUMA)) %>%
  select( -SUMA ) %>%
  filter( ! is.na(CODIGO) ) %>%
  distinct() %>%
  arrange(CODIGO) %>%
  #
  mutate( FECHA = fecha_corte ) %>%
  select( FECHA, everything() )

# ----

SB_[2, 3:36] %>% sum(na.rm = TRUE)
SB_[, 3:36] %>% rowSums(na.rm = TRUE)
sum(SB_[2,-c(1,2)],na.rm = TRUE)


# esto es para tabla derretida

SB_derretido <- SB_ %>%
  reshape2::melt(.,
                 id.vars = colnames(.)[1:3],
                 variable.name = "RAZON_SOCIAL",
                 value.name = "VALOR") %>%
  group_by(FECHA, RAZON_SOCIAL, CODIGO, CUENTA) %>%
  #filter(if (any(is.na(VALOR)) & n() > 1) !is.na(VALOR) else TRUE) %>%
  ungroup() %>%
  agregarRUCenSB() %>%
  select("FECHA", "CODIGO", "CUENTA", "RUC", "RAZON_SOCIAL", "VALOR")




