analisisDifusoNLPFechaCorte <- function(tabla) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.
  
  # Paquete para manejo de fechas
  if (!require("lubridate")) { 
    install.packages("lubridate")
    library(lubridate)
  }
  
  # Funciones de las pruebas correspondientes
  traductor_mes <- function(texto) {
    texto_original <- tolower(texto)
    meses <-
      data.frame(
        es = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"),
        #en = substr(strsplit(tolower(month.name), " "), 1, 3)
        en = tolower(month.name)
      )
    patron <- meses$es
    reemplazo <- meses$en
    separadores <- "[-,/, ]"
    #palabras <- unlist(strsplit(texto_original, separadores))
    
    # IDEA: Se podría dividir cada palabra y reunificar sus 3 primeras letras para coincidencia con el máximo
    palabras <- strsplit(texto_original, separadores)[[1]]
    palabras_abreviadas <- substr(palabras, 1, 3)
    similitudes <- stringdist::stringsimmatrix(palabras_abreviadas, patron, method = "jw")
    
    #similitudes <- stringdist::stringsimmatrix(palabras, patron, method = "jw")
    # Se emplea una probabilidad de similitud del 90% para compensar el error por identidad con el máximo
    #posiciones_max <- as.data.frame(which(similitudes >= 0.8*max(similitudes), arr.ind = TRUE))
    posiciones_max <- as.data.frame(which(similitudes == max(similitudes), arr.ind = TRUE))
    palabra_similar <- palabras[posiciones_max$row]
    reemplazo_similar <- reemplazo[posiciones_max$col]
    #texto_modificado <- gsub(palabra_similar, reemplazo_similar, texto_original)
    texto_modificado <- texto_original
    # introducimos uno a uno el nombre del mes traducido
    for ( k in seq_along(posiciones_max) ) {
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
        #"\\b(20[0-9]{2}[-/][[:alpha:]]{1,10}[-/][0-3]?[0-9])\\b",
        "\\b([0-3]?[0-9][-/][0-1][0-9][-/]20[0-9]{2})\\b" #,
        #"\\b([0-3]?[0-9][-/][[:alpha:]]{1,10}[-/]20[0-9]{2})\\b"
        ), collapse = "|"
      )
    return(grepl(expresion_regular_fecha_date, texto))
  }
  
  # Empleamos la función indicePrimeraFilDecimalTabla()
  indice_fila_nombres <- indicePrimeraFilDecimalTabla(tabla)
  # Subtabla previa a los valores decimales, y ala fila de nombres de columnas, por eso se resta 2
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
    cat("\nNo se pudo encontrar una fehca.\n")
    break
  } 
  
  if ( prueba_fecha_date(contenido_celda) ) {
    
    fecha_identificada <- parsedate::parse_date(traductor_mes(contenido_celda))
  
  # Establecemos la condición para cuando el texto leído corresponde a fecha en formato numérico de Excel no
    
  } else if ( prueba_fecha_excel(contenido_celda) ) {
    
    # Determinamos el valor de la celda buscada con la fecha de corte
    num_fecha_corte <- as.numeric(contenido_celda)
    # Determinamos la fecha de corte
    fecha_identificada <- as.Date( num_fecha_corte, origin = "1899-12-30")
    
  } else if ( prueba_anio(contenido_celda) & prueba_mes(contenido_celda) ) {
    
    # Dividimos el texto original en sus componentes por si hubiera más de una fecha
    texto_dividido <- unlist(strsplit(contenido_celda, " "))
    
    if ( sum(prueba_anio(texto_dividido)) == 1 ) {
      fechas_reconocidas <- traductor_mes(contenido_celda)
      fecha_identificada <- parsedate::parse_date(fechas_reconocidas)
    } else {
      fechas_reconocidas <- traductor_mes(contenido_celda)
      fechas_reconocidas <- strsplit(fechas_reconocidas, " ")[[1]]
      #fechas_reconocidas <- traductor_mes(texto_dividido)
      fechas_reconocidas <- parsedate::parse_date(fechas_reconocidas)
      fecha_identificada <- fechas_reconocidas[which.max(fechas_reconocidas)]
    }
    #fecha_identificada <- parsedate::parse_date(fechas_reconocidas)
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

tabla <- lista_hojasPYG[[1]]
analisisDifusoNLPFechaCorte(tabla)

analisisDifusoNLPFechaCorte(lista_hojasPYG[[7]])

View(t(list2DF(lapply(head(lista_hojasPYG,40), analisisDifusoNLPFechaCorte))))


# 2015-07-31 hasta esa fecha funciona bien
