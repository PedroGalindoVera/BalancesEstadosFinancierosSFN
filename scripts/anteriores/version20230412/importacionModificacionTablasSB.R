eliminarColumnasNumeracion <- function(tabla) {
  
  # Esta función permite identificar y eliminar columna con numeración de filas en una tabla.
  
  # Ejemplo de uso: tabla <- eliminarColumnasNumeracion(tabla)
  
  # Especificamos la tolerancia de encontrar un conjunto inductor
  probabilidad <- 0.75
  
  # Determinamos si una columna contiene datos numéricos secuenciales dada la tolerancia
  columnas_inductoras <- sapply(tabla, function(x) is.numeric(na.omit(x)) && mean(diff(na.omit(x)) == 1) > probabilidad )
  # Identificamos el indice de las columnas buscadas
  indices_columnas_inductoras  <- as.numeric(which(columnas_inductoras))
  # Ver datos a eliminar
  # View(mi_df[,indice_columnas_inductoras])
  # Eliminamos las columnas inductoras
  if ( length(indices_columnas_inductoras) > 0 ) {
    cat("Se ha(n) elimando", length(indices_columnas_inductoras), "columna(s) de numeración de filas.\n")
    return( tabla[, -indices_columnas_inductoras] )
  } else {
    cat("No se han encontrado columnas de numeración de filas.\n")
    return( tabla )
  }
}

eliminarColumnasInsignificantes <- function(tabla) {
  
  # Esta función permite identificar y eliminar columnas que contienen únicamente valores NA o sin significancia.
  
  # NOTA: Se recomienda emplear esta función luego de emplear eliminarColumnasNumeracion()
  
  # Ejemplo de uso: tabla <- eliminarColumnasInsignificantes(tabla)
  
  # Identificamos lógicamente las columnas que contienen únicamente NA o  son mayores 0 y menores a 10
  conlumnas_insignificantes <- apply(tabla, 2, function(x) all(is.na(x) | between(as.numeric(x),1,10) ) )
  # Obtenemos los indices de las filas identificadas
  indices_conlumnas_insignificantes <- as.numeric(which(conlumnas_insignificantes))
  # Ver datos a eliminar
  # View(tabla[indices_conlumnas_insignificantes,])
  # Excluimos las filas identificadas
  if ( length(indices_conlumnas_insignificantes) > 0 ) {
    cat("Se ha(n) elimando", length(indices_conlumnas_insignificantes), "columna(s) insignificante(s).\n")
    return( tabla[,-indices_conlumnas_insignificantes] )
  } else {
    cat("No se han encontrado columnas insignificantes.\n")
    return( tabla )
  }
}

eliminarFilasSinValores <- function(tabla) {
  
  # Esta función permite identificar y eliminar filas que contienen únicamente valores NA o 0 en una tabla.
  
  # NOTA: Se recomienda emplear esta función luego de emplear eliminarColumnasNumeracion()
  
  # Ejemplo de uso: tabla <- eliminarFilasSinValores(tabla)
  
  # Identificamos las filas que contienen únicamente NA o 0
  filas_sin_valores <- apply(tabla, 1, function(x) all(is.na(x) | x == 0))
  # Obtenemos los indices de las filas identificadas
  indices_filas_sin_valores <- which(filas_sin_valores)
  # Ver datos a eliminar
  # View(tabla[indices_filas_sin_valores,])
  # Excluimos las filas identificadas
  if ( length(indices_filas_sin_valores) > 0 ) {
    cat("Se ha(n) elimando", length(indices_filas_sin_valores), "fila(s) sin valores.\n")
    return( tabla[-indices_filas_sin_valores,] )
  } else {
    cat("No se han encontrado filas sin valores.\n")
    return( tabla )
  }
}

eliminarColumnasNA <- function(tabla) {
  
  # Esta función permite identificar y eliminar columnas que contienen únicamente valores NA o 0 en una tabla.
  
  # NOTA: Se recomienda emplear esta función luego de emplear eliminarColumnasNumeracion()
  
  # Ejemplo de uso: tabla <- eliminarcolumnasSinValores(tabla)
  
  # Identificamos las columnas que contienen únicamente NA o 0
  columnas_sin_valores <- apply(tabla, 1, function(x) all(is.na(x)))
  # Obtenemos los indices de las columnas identificadas
  indices_columnas_sin_valores <- which(columnas_sin_valores)
  # Ver datos a eliminar
  # View(tabla[indices_columnas_sin_valores,])
  # Excluimos las columnas identificadas
  if ( length(indices_columnas_sin_valores) > 0 ) {
    cat("Se ha(n) elimando", length(indices_columnas_sin_valores), "columna(s) sin valores.\n")
    return( tabla[-indices_columnas_sin_valores,] )
  } else {
    cat("No se han encontrado columnas NA\n")
    return( tabla )
  }
}

eliminarInformacionFinTabla <- function(tabla) {
  
  # Esta función permite identificar y eliminar las filas con información ajena a los datos y presente en la parte inferior de la tabla.
  
  # Ejemplo de uso: tabla <- eliminarInformacionFinTabla(tabla)
  
  # Lista de palabras a buscar
  lista_palabras <- c("prueba de cuadre", "fuente", "elaboración", "nota", "grandes", "medianas", "pequeñas")
  
  # Identificamos lógicamente las columnas con coincidencias
  numero_coincidencia_columna <- apply(tabla, 2, function(x) sum(grepl(paste(lista_palabras, collapse = "|"), x, ignore.case = TRUE)) )
  # Determinamos la primera columna con mayores coincidencias
  indice_columna_mayores_coincidencias <- which.max(numero_coincidencia_columna)[1]
  # Identificamos lógicamente las filas con coincidencias en la columna antes identificada
  coincidencias_filas <- grepl(paste(lista_palabras, collapse = "|"), tabla[[indice_columna_mayores_coincidencias]], ignore.case = TRUE)
  # Obtenemos los indices de las filas identificadas
  indices_filas_coincidencias <- which(coincidencias_filas)
  # Especificamos el índice de la fila referencial para eliminar la información
  indice_fila_referencial <- min(indices_filas_coincidencias) - 1
  # Ver datos a eliminar
  # View(tabla[(indice_fila_referencial+1):nrow(tabla),])
  # Verificamos que se trata de las últimas filas antes de eliminarlas
  if ( indice_fila_referencial > nrow(tabla) - 20 ) {
    indice_fila_referencial <- min(indice_fila_referencial - 1, nrow(tabla))
    cat("Se ha(n) elimando", nrow(tabla) - indice_fila_referencial, "fila(s) con meta data al final de la tabla.\n")
    return( tabla[1:indice_fila_referencial, ] )
  } else {
    cat("No se han encontrado filas por eliminar al final de la tabla.\n")
    return( tabla )
  }
}

identificarIndiceFilaNombres <- function(tabla) {
  
  # Esta función retorna el índice de la columna de nombres de una tabla, asociada a una lista de nombres a buscar.
  
  # Ejemplo de uso: tabla <- identificarIndiceFilaNombres(tabla)

  # Lista de nombres a buscar
  lista_nombres_bancos <- c("cuenta", "codigo", "pichincha", "guayaquil", "produbanco", "internacional", "loja", "austro", "machala", "pacifico")
  # Especificamos el número mínimo de coincidencias a buscar
  numero_minimo_coincidencias <- 1
  
  # Determinamos el número de coincidencias por cada fila
  numero_coincidencias_fila <- apply(tabla, 1, function(x) sum(grepl(paste(lista_nombres_bancos, collapse = "|"), x, ignore.case = TRUE)) )
  # Identificamos el índice de las filas que cumplen el mínimo de coincidencias
  indices_filas_minimas_coincidencias <- which(numero_coincidencias_fila >= numero_minimo_coincidencias)
  # Identificamos el índice de las filas que tienen mayores coincidencias
  indices_fila_mayores_coincidencias <- which.max(numero_coincidencias_fila)
  # Determinamos la primera fila que cumple ambas condiciones
  indice_fila_referencial <- intersect(indices_filas_minimas_coincidencias, indices_fila_mayores_coincidencias)[1]
  
  return( indice_fila_referencial )
}

analisisDifusoNLPFechaCorte <- function(texto) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.
  
  # Paquete para procesamiento de lenguaje natural
  if (!require("parsedate")) { 
    install.packages("parsedate")
    library(parsedate)
  }
  
  #if ( !grepl("^[0-9]+$", texto) ) {
  #if (grepl("[0-9]", texto) & grepl("[a-zA-Z]", texto)) {
  # Verificamos si la expresión incluye un número de 4 digitos correspondiente a un año y texto únicamente con símbolos de letras correspondiente a un mes
  if (grepl("[0-9]{4}", texto) & grepl("[a-zA-Z]", texto)) {
    
    # Paquete para análisis difuso de texto
    if (!require("stringdist")) { 
      install.packages("stringdist")
      library(stringdist)
    }
    
    # Definimos un diccionario para consulta, en un data frame
    diccionario_meses <- data.frame(
      fecha = seq.Date(as.Date("0000-01-01"), as.Date("0000-12-31"), by = "month"),
      # La consulta del mes se realiza en ingles por que es muchísimo más precisa
      ingles = tolower(month.name)
    )
    diccionario_meses$espaniol <- format(diccionario_meses$fecha, "%B")
    #diccionario_meses$abreviacion <- gsub("\\.", "", format(diccionario_meses$fecha, "%b"))
    
    # Análisis difuso de texto
    texto_buscado <- tolower(texto)
    distancias <- stringdist::stringdist(diccionario_meses$espaniol, texto_buscado, method = "lv")
    traduccion_mes <- diccionario_meses$ingles[distancias <= min(distancias)][1]
    
    # Determinamos el año
    anio <- format(parsedate::parse_date(texto), "%Y")
    # Determinamos el mes
    mes <- format(parsedate::parse_date(traduccion_mes), "%m")
    # Definimos un string con una fecha preliminar 
    fecha_corte_preliminar <- paste(anio,mes,"01",sep = "-")
    # Corregimos la fecha para fin de mes o retornamos NA por no encontrar año
    if ( !is.na(anio) ) {
      # Paquete para manejo de fechas
      if (!require("lubridate")) { 
        install.packages("lubridate")
        library(lubridate)
      }
      # Determinamos el último día del mes encontrado
      fecha_corte <- as.Date(fecha_corte_preliminar) + months(1) - days(1)
    } else {
      fecha_corte <- NA
    }
    #return(fecha_corte)
    
  } else if (grepl("^[0-9]{4}[-/][0-9]{2}[-/][0-9]{2}$", texto) | grepl("^[0-9]{2}[-/][0-9]{2}[-/][0-9]{4}$", texto)) {
    
    fecha_corte <- parsedate::parse_date(texto)
    
  } else {
    
    fecha_corte <- NA
    
  }
  
  return(fecha_corte)
}

identificarFechaCorteBoletinSB <- function(tabla) {
  
  # Esta función retorna la fecha de corte del Boletín Mensual SB.
  
  # NOTA: Requiere emplear la función identificarIndiceFilaNombres()
  
  # Ejemplo de uso: tabla <- identificarFechaCorteBoletinSB(tabla)
  
  # Función para transformar a formato numérico de Excel una fecha date
  formato_numerico_excel <- function(fecha) {
    # Fecha base de Excel
    fecha_base_excel <- as.Date("1899-12-30")
    return(as.numeric(difftime(as.Date(fecha), as.Date("1899-12-30"))))
  }
  # Número correspondiente a fecha inicial para intervalo de búsqueda
  num_fecha_inicio <- formato_numerico_excel("2000-01-01")
  # Número correspondiente a fecha final para intervalo de búsqueda
  num_fecha_fin <- formato_numerico_excel("2100-01-01")
  # Empleamos la función identificarIndiceFilaNombres()
  indice_fila_nombres <- identificarIndiceFilaNombres(tabla)
  # Subtabla
  subtabla <- tabla[1:indice_fila_nombres-1,]
  # Identificamos los indices de las celdas de la tabla que están en el rango numérico determinado antes de la fila con los nombres de columnas
  celdas_coincidencias <- which( subtabla >= num_fecha_inicio & subtabla <= num_fecha_fin, arr.ind = TRUE)
  
  if ( length(celdas_coincidencias) > 0 ) {
    # Determinamos el valor de la celda buscada con la fecha de corte
    num_fecha_corte <- as.numeric(tabla[celdas_coincidencias[1,1], celdas_coincidencias[1,2]])
    # Determinamos la fecha de corte
    fecha_corte_date <- as.Date( num_fecha_corte, origin = "1899-12-30")
  } else {
    # Entradas de la subtabla
    entradas <- as.character( unlist(subtabla[!is.na(subtabla)]) )
    # Determinamos las fechas asociadas a las entradas
    num_fechas <- lapply(entradas, analisisDifusoNLPFechaCorte)
    # Tansformamos a date
    fecha_corte_date <- num_fechas[!is.na(num_fechas)][[1]]
  }
  
  return( fecha_corte_date[1] )
}

crearTablaBoletinMensualSB <- function(tabla) {
  
  # Esta función crea una tabla de campos con la tabla que recibe
  
  # NOTA: Requiere emplear la función identificarIndiceFilaNombres()
  
  # Ejemplo de uso: nueva_tabla <- crearTablaBoletinMensualSB(tabla)
  
  # Empleamos la función `identificarIndiceFilaNombres()` para obtener el indice de fila correspondiente
  indice_fila_nombres <- identificarIndiceFilaNombres(tabla)
  # Creamos la nueva tabla a partir de la fila antes especificada
  #nueva_tabla <- tabla[indice_fila_nombres+1:nrow(tabla), ]
  nueva_tabla <- tabla[-(1:indice_fila_nombres), ]
  # Asignamos los nombres de las columna
  colnames(nueva_tabla) <- as.character(tabla[indice_fila_nombres,])
  # Generar nombres únicos a partir de los nombres originales de las columnas para columnas sin nombre
  colnames(nueva_tabla) <- make.unique(names(nueva_tabla))
  
  return(nueva_tabla)
}

identificarTipoDatoColumnaSB <- function(columna, tamanho_muestra = 100, probabilidad_ocurrencia = 0.8) {
  
  # Esta función permite identificar y cambiar el tipo de dato de una columna.
  
  # Ejemplo de uso: columna <- identificarTipoDatoColumnaSB(columna, tamanho_muestra = 1000, probabilidad_ocurrencia = 0.8)
  
  # Determinamos una muestra en las primeras columnas de la tabla
  tamanho_columna <- length(columna)
  if ( tamanho_muestra < 0.1*tamanho_columna ) tamanho_muestra <- round(0.25*tamanho_columna)
  columna_muestra <- head(columna, tamanho_muestra)
  
  # Determinamos expresiones regulares para excluir expresiones con puntos y solo admitir con números
  expresion_regular_codigo <- "^[[:digit:]]{1,6}$"
  expresion_regular_decimal <- "^([.,][0-9]+)?$"
  # Determinamos la prueba lógica para código
  prueba_codigo <-
    ( grepl(expresion_regular_codigo, columna_muestra) &
        !grepl(expresion_regular_decimal, columna_muestra) ) |
    is.na(columna_muestra)
  # Determinamos expresiones regulares para identificar números con o sin decimales
  expresion_regular_numero <- "^[0-9]+([.,][0-9]+)?$"
  # Determinamos la prueba lógica para números
  prueba_numero <- grepl(expresion_regular_numero, columna_muestra) | is.na(columna_muestra)
  # Determinamos expresiones regulares para texto
  expresion_regular_texto <- "[[:alpha:]]"
  # Determinamos la prueba lógica para texto
  prueba_texto <- grepl(expresion_regular_texto, columna_muestra) | is.na(columna_muestra)
  # Determinamos expresiones regulares para texto NA como palabra completa
  expresion_regular_na <- "(?i)\\bna\\b"
  # Determinamos la prueba lógica para texto na
  prueba_na <- all(grepl(expresion_regular_na, columna_muestra) | is.na(columna_muestra))
  # Asumiendo distribucion Poisson para cada caso, calculamos un estimador para la media que representa la tasa media de ocurrencia
  media_codigo <- mean(prueba_codigo, na.rm = TRUE)
  media_numero <- mean(prueba_numero, na.rm = TRUE)
  media_texto <- mean(prueba_texto, na.rm = TRUE)
  
  # Determinamos la mayor probabilidad de ocurrencia
  if ( media_numero >= probabilidad_ocurrencia & media_numero > media_codigo & media_codigo >= media_texto ) {
    if ( class(columna) != "numeric" ) columna <- as.numeric(columna)
  } else if ( media_codigo >= probabilidad_ocurrencia & media_codigo > media_numero & media_numero >= media_texto  ) {
    if ( class(columna) != "character" ) columna <- as.character(columna)
  } else if ( media_texto >= probabilidad_ocurrencia & media_texto > media_codigo & media_codigo >= media_numero ) {
    if ( class(columna) != "character" ) columna <- as.character(columna)
  } else if ( prueba_na ) {
    if ( class(columna) != "numeric" ) columna <- as.numeric(columna)
  }
  
  return(columna)
}

identificarTipoDatoTablaSB <- function(tabla, tamanho_muestra = 100, probabilidad_ocurrencia = 0.8) {
  
  # Esta función permite identificar y cambiar el tipo de dato de una columna, empleando la función para la misma tarea en columna
  
  # Ejemplo de uso: tabla <- identificarTipoDatoTablaSB(columna, tamanho_muestra = 1000, probabilidad_ocurrencia = 0.8)
  
  k <- 1
  for (columna in tabla) {
    tabla[k] = identificarTipoDatoColumnaSB(columna, tamanho_muestra = 100, probabilidad_ocurrencia = 0.8)
    k <- k + 1
  }
  
  return(tabla)
}

hojasLibrosExcelDirectorio <- function(ruta_directorio) {
  
  # Esta función permite obtener los nombres de todas las hojas de todos los libros de excel en un directorio.
  
  # Ejemplo de uso: hojasLibrosExcelDirectorio(ruta_directorio = "data/Fuente/SB")
  
  # Requerimiento de paquetes
  if (!require("readxl")) { 
    install.packages("readxl")
    library(readxl)
  }
  
  # Listamos todos los archivos del directorio especificado
  archivos <- list.files(ruta_directorio, recursive = TRUE)
  
  #cat("Analisando las hojas de cálculo en los libros de excel del directorio [", ruta_directorio, "]:\n")
  
  # Inicializamos una lista para almacenar los nombres de las hojas de cálculo
  lista_nombres_hojas <- list()
  
  for (archivo in archivos) {
    contador <- if ( archivo == archivos[[1]] ) 1 else contador + 1
    porcentaje <- round(contador / length(archivos) * 100)
    cat("Analisando las hojas de cálculo en los libros de excel del directorio [", ruta_directorio, "]: ", porcentaje, "% completado\r")
    #cat(porcentaje, "% completado\r")
    # Definimos la ruta de cada libro de excel
    ruta_archivo <- file.path(ruta_directorio, archivo)
    # Verificamos que el archivo sea un libro de Excel
    if ( grepl("\\.xlsx$|\\.xls$|\\.xl.*$", archivo) ) {
      # Identificamos todas las hojas de un libro de excel
      nombre_hoja <- excel_sheets(ruta_archivo)
      # Incorporamos el nombre de todas las hojas a la lista
      lista_nombres_hojas[[ruta_archivo]] <- nombre_hoja
    } else {
      cat("El archivo: [", ruta_archivo, "] no es una hoja de cálculo admisible.\n")
    }
  }
  
  # Determinamos los nombres presentes en la lista
  nombres_hojas <- sort(unique(as.character(unlist(lista_nombres_hojas))))
  
  # Determinamos el número mayor de hojas entre los libros
  max_length <- max(sapply(lista_nombres_hojas, length))
  # Completamos con valores NA para poder formar filas con el mismo número de "hojas"
  data_padded <- lapply(lista_nombres_hojas, function(x) {c(x, rep(NA, max_length - length(x)))})
  # Creamos una tabla que resume toda la información
  tabla <- as.data.frame(data_padded, stringsAsFactors = FALSE)
  tabla <- as.data.frame(t(tabla))
  colnames(tabla) <- paste("Hoja",seq(1,max_length))
  
  return(
    hojas <- list(lista = lista_nombres_hojas, tabla = tabla, nombres = nombres_hojas)
  )
}

selectorRutasHojasLibrosDirectorio <- function(ruta_directorio, nombre_hoja_buscar, anio_inicio) {
  
  # Esta función retorna las rutas de los libros con las hojas a partir de un año espedificado
  
  # Ejemplo de uso: selectorRutasHojasLibrosDirectorio(ruta_directorio = "data/Fuente/SB", nombre_hoja_buscar = "balance", anio_inicio = 2013)
  
  # NOTA: Se emplea la función `hojasLibrosExcelDirectorio` para determinar la ruta de las hojas de interés.
  
  # Llamamos a la función para crear el objeto hojas con la información de todos los libros en el directorio
  hojas <- hojasLibrosExcelDirectorio(ruta_directorio = ruta_directorio)
  # Identificamos lógicamente los libros que contienen una Hoja con el nombre `nombre_hoja_buscar`
  contiene_hoja_buscada <- sapply(hojas$lista, function(x) any(grepl(nombre_hoja_buscar, x, ignore.case = TRUE)))
  # Especificamos los indices correspondientes
  indices <- which(contiene_hoja_buscada)
  # Obtenemos las curas correspondientes
  rutas_libros_hojas_buscadas <- names(hojas$lista)[indices]
  # Determinamos un vector de búsqueda para la condición temporal
  fechas_admisibles <- paste(seq(anio_inicio,2100,1), collapse = "|")
  # Identificamos lógicamente las rutas de archivos que cumplen la fecha admisible
  condicion_temporal <- sapply(rutas_libros_hojas_buscadas, function(x) any(grepl(fechas_admisibles, x, ignore.case = TRUE)))
  # Determinamos las rutas de los libros con las hojas buscadas
  rutas_seleccionadas <- rutas_libros_hojas_buscadas[condicion_temporal]
  
  return(rutas_seleccionadas)
}

identificadorTablaNombreIF <- function(catalogo = NULL, tabla) {
  
  # Función para identificar los nombres de las columnas de una tabla utilizando un catálogo de operadores y el método de similitud de cadenas Jaro-Winkler
  
  # Ejemplo de uso: tabla <- identificadorTablaNombreIF(tabla = tabla)
  
  if ( is.null(catalogo) ) {
    # Requerimiento de paquetes
    if (!require("readxl")) { 
      install.packages("readxl")
      library(readxl)
    }
    # Leemos el catalogo de una libro de Excel
    catalogo <- readxl::read_excel("data/Otros/Catálogo Operadores.xlsx")
    # Se agrega 0 como número y "0" como caracter para  para nombres inadecuados en columnas
    catalogo_complemento <- data.frame(
      RUC = rep("0"),
      Operadora = c(as.character(seq(0,9)),
                    "NA", "FECHA", "CODIGO", "CUENTA",
                    "BANCA MULTIPLE",
                    "BANCOS PRIVADOS GRANDES",
                    "BANCOS PRIVADOS MEDIANOS",
                    "BANCOS PRIVADOS PEQUEÑOS",          
                    "BANCOS PRIVADOS COMERCIALES",
                    "BANCOS PRIVADOS CONSUMO",      
                    "BANCOS PRIVADOS VIVIENDA",
                    "BANCOS PRIVADOS DE MICROEMPRESA",
                    "BANCOS PRIVADOS DE MICROCREDITO",
                    "TOTAL BANCOS PRIVADOS"))
    catalogo <- rbind(catalogo_complemento, catalogo)
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
  # Eliminar las columnas con nombres inadecuados
  indice_columnas_eliminar <- which( names(tabla) %in% c("NA", as.character(seq(1,9))) )
  tabla <- tabla[,-indice_columnas_eliminar]
  # Notificar las columnas eliminadas
  if ( length(indice_columnas_eliminar) > 0  ) {
    cat("Se han eliminado las siguientes columnas mal identificadas: [", nombres_columnas[indice_columnas_eliminar],"]\n")
  } else {
    cat("No se ha encontrado ninguna columna mal identificada.\n")
  }
  return(tabla)
}

impresionProgreso <- function(conjunto) {
  contador_progreso <<- if ( exists("contador_progreso") ) contador_progreso + 1 else 1
  porcentaje_progreso <- round(contador_progreso / length(conjunto) * 100, 1)
  cat(paste0("\n[",contador_progreso,"]"), porcentaje_progreso, "% completado\n")
  if ( contador_progreso == length(conjunto) ) rm(contador_progreso, envir = .GlobalEnv)
}

listaImportacionDatosFuenteSB <- function(nombre_hoja_buscada) {
  
  # Esta función importar como tablas las hojas con los "Balances Financieros" de los libros con los "Boletines Financieros mensuales" de la SB
  
  #### Determinación Hojas de interés ####
  # Periodos de empleo del nombre para la Hoja con el "Balance Financiero" en los "Boletines Financieros mensuales"
  # "BAL": [2001,2002,2011,2012]
  # "BAL SAB II": [2008-2011]
  # "BALANCE": [2012, ...[
  
  # NOTA. Se emplea las funciones: `selectorRutasHojasLibrosDirectorio` y otras más por lo que se debe llamar a:
  #source("scripts/importacionModificacionTablasSB.R")
  
  # Se decide leer únicamente las Hojas "BALANCE" desde 2013 por que tienen una plantilla más homogénea
  rutas_balances_SB <- 
    selectorRutasHojasLibrosDirectorio(ruta_directorio = "data/Fuente/SB", nombre_hoja_buscar = "balance", anio_inicio = 2013)
  
  # Requerimiento de paquetes
  if (!require("readxl")) { 
    install.packages("readxl")
    library(readxl)
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require("stringdist")) { 
    install.packages("stringdist")
    library(stringdist)
  }
  
  if( exists("contador_progreso") ) rm(contador_progreso)
  
  lista_tablas <- list()
  
  for ( ruta in rutas_balances_SB){
    impresionProgreso(rutas_balances_SB)
    cat("Leyendo el archivo: [", ruta, "]\n")
    # Determinamos los nombres de las hojas de cada libro de Excel
    nombres_hojas <- readxl::excel_sheets(ruta)
    # Análisis difuso de texto
    distancias <- stringdist::stringdist(tolower(nombres_hojas), tolower(nombre_hoja_buscada), method = "lv")
    nombre_hoja_coincidencia <- nombres_hojas[distancias <= min(distancias)][1]
    nombre_tabla <- paste(basename(ruta),nombre_hoja_buscada)
    # Importación de la hoja buscada como data frame
    tabla <- readxl::read_excel(ruta, sheet = nombre_hoja_coincidencia)
    # Leemos la fecha del corte en cada tabla
    fecha_corte <- identificarFechaCorteBoletinSB(tabla)
    # Creamos la tabla de datos propiamente
    lista_tablas[[nombre_tabla]] <-
      crearTablaBoletinMensualSB(tabla) %>%
      eliminarInformacionFinTabla() %>%
      identificarTipoDatoTablaSB() %>%
      eliminarFilasSinValores() %>%
      eliminarColumnasNA() %>%
      mutate(`FECHA` = rep(fecha_corte)) %>%
      select(`FECHA`, everything()) %>%
      identificadorTablaNombreIF(tabla = .)
  }
  
  return(lista_tablas)
}
