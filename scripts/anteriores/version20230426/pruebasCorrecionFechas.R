analisisDifusoNLPFechaCorte0 <- function(texto) {
  
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
    separadores <- "[_,-,/, ]"
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
  
  # Establecemos el proceso directo para formatos de fecha
  
  contenido_celda <- as.character(texto)
  
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
  } else {
    
    cat("\nNo se ha podido identificar ninguna fecha.\n")
    
  }
  
  if ( exists("fecha_identificada") ) {
    # Determinamos el año
    anio <- format(fecha_identificada, "%Y")
    # Determinamos el mes
    mes <- format(fecha_identificada, "%m")
    # Determinamos una fecha preliminar
    fecha_corte_preliminar <- paste(anio,mes,"01",sep = "-")
    # Determinamos el último día del respectivo mes
    fecha_corte <- as.Date(fecha_corte_preliminar) + months(1) - lubridate::days(1)
  } else {
    fecha_corte <- NA
  }
  
  return(fecha_corte)
}

tablaFechasBAL <- verificadorReconocimientoFechas(lista_hojasBAL)

nombres_irreconocidos <- row.names(fechasPYG)[fechasPYG$diferencia_meses <= 26]
analisisDifusoNLPFechaCorte0(nombres_irreconocidos[2])
traductor_mes(nombres_irreconocidos[2])

parse_date(gsub("_"," ", traductor_mes(nombres_irreconocidos[2])))
parse_date(gsub("_"," ", traductor_mes(nombres_irreconocidos[3])))

fecha <- list()
for ( k in seq_along(names(lista_hojasPYG))) {
  fecha[[k]] <- parse_date(gsub("_"," ", traductor_mes(names(lista_hojasPYG)[k])))
}

fechasBF  <- data.frame(BAL = fechasBAL$fecha, PYG = fechasPYG$fecha)
fechasBF$concidencia <- fechasBF$BAL == fechasBF$PYG
fechasBF$diferencia <- difftime(fechasBF$BAL,fechasBF$PYG,units = "days")
