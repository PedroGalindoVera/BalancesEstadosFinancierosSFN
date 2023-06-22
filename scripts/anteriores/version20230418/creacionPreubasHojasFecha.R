analisisDifusoNLPFechaCorte <- function(texto) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.
  
  # Paquete para procesamiento de lenguaje natural
  if (!require("parsedate")) { 
    install.packages("parsedate")
    library(parsedate)
  }
 
  # Verificamos si la expresión incluye un número de 4 dígitos que pueda coincidir con un año
  #prueba_anio <- grepl("[0-9]{4}", texto)
  prueba_anio <- grepl("[[:digit:]]{4}", texto)
  # Verificamos si la expresión incluye texto únicamente con símbolos de letras, que pueda coincidir con el mes
  prueba_texto <- grepl("[[:alpha:]]", texto)
  
  # Exigimos las dos coincidencias anteriores
  if (prueba_anio & prueba_texto) {
    
    # Paquete para análisis difuso de texto
    if (!require("stringdist")) { 
      install.packages("stringdist")
      library(stringdist)
    }
    
    # # Definimos un diccionario para consulta, en un data frame
    # diccionario_meses <- data.frame(
    #   fecha = seq.Date(as.Date("0000-01-01"), as.Date("0000-12-31"), by = "month"),
    #   # La consulta del mes se realiza en ingles por que es muchísimo más precisa
    #   ingles = tolower(month.name)
    # )
    # diccionario_meses$espaniol <- format(diccionario_meses$fecha, "%B")
    # #diccionario_meses$abreviacion <- gsub("\\.", "", format(diccionario_meses$fecha, "%b"))
    # 
    # # Análisis difuso de texto
    # texto_buscado <- tolower(texto)
    # distancias <- stringdist::stringdist(diccionario_meses$espaniol, texto_buscado, method = "lv")
    # traduccion_mes <- diccionario_meses$ingles[distancias <= min(distancias)][1]
    
    
    # Dividimos el texto original en sus componentes por si hubiera más de una fecha
    texto_dividido <- strsplit(as.character(texto), " ")
    # Determinamos las fechas almacenadas con el paquete parsedate y manipulamos para obtener un solo vector de fechas
    fechas_reconocidas <- lapply(texto_dividido, function(componente) as.Date(parsedate::parse_date(componente)))
    fechas_reconocidas <- fechas_reconocidas[[1]]
    fechas_reconocidas <- fechas_reconocidas[!is.na(fechas_reconocidas)]
    # Determinamos el año
    anio <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%Y")))[1]
    # Determinamos el mes
    mes <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%m")))[1]

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
      cat("\nNo se pudo determinar el año.\n")
      fecha_corte <- NA
    }
    #return(fecha_corte)
    
  } else if (grepl("[0-9]{4}[-/][0-9]{2}[-/][0-9]{2}$", texto) | grepl("[0-9]{2}[-/][0-9]{2}[-/][0-9]{4}$", texto)) {
    
    fecha_corte <- parsedate::parse_date(texto)
    
  } else {
    
    cat("\nNo se pudo encontrar ningna fecha.\n")
    fecha_corte <- NA
    
  }
  #return(fecha_corte)
  return(as.Date(fecha_corte))
}

identificarFechaCorteBoletinSB <- function(tabla) {
  
  # Esta función retorna la fecha de corte del Boletín Mensual SB.
  
  # NOTA: Requiere emplear la función indicePrimeraFilDecimalTabla()
  
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
  # Empleamos la función indicePrimeraFilDecimalTabla()
  indice_fila_nombres <- indicePrimeraFilDecimalTabla(tabla) - 1
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

tabla <- lista_hojasPYG[[row.names(fechasPYG)[1]]]
View(tabla)
texto <- tabla[3,2]

analisisDifusoNLPFechaCorte(texto)

lista_fechasPYG <- sapply(lista_hojasPYG, identificarFechaCorteBoletinSB)
fechasPYG  <- as.data.frame( as.Date(lista_fechasPYG, origin = "1970-01-01") )





tabla <- lista_hojasBAL[[107]]


#### INNOVACION
analisisDifusoNLPFechaCorte <- function(tabla) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.

  # Paquete para análisis difuso de texto
  if (!require("stringdist")) { 
    install.packages("stringdist")
    library(stringdist)
  }
  
  # Empleamos la función indicePrimeraFilDecimalTabla()
  indice_fila_nombres <- indicePrimeraFilDecimalTabla(tabla)
  # Subtabla
  subtabla <- tabla[1:(indice_fila_nombres-2),]
  
  
  # idea para la expresion regular de año, usar Sys.Date() y descomponer
  anio_num <- year(Sys.Date())
  anio_text <- strsplit(as.character(anio_num), split = "")[[1]]
    
  # Establecemos una expresión regular que acepta 2000 hasta el año actual
  expresion_regular_anio <- paste0("(",anio_text[1],"[",0,"-",anio_text[2],"][",0,"-",anio_text[3],"][",0,"-",anio_text[4],"])$")
  # expresion_regular_anio <- "(2[0][0-3][0-9])$" # acepta desde 2000 hasta 2039
  #expresion_regular_anio <- "[[:digit:]]{4}"
  
  # Establecemos una expresión regular
  expresion_regular_mes <- "[[:alpha:]]"
  # Establecemos una expresión regular para identificar fechas en formato numérico de Excel desde el 2000 hasta 2036
  # expresion_regular_fecha_num_excel <- "(3[5-9][0-9]{3}|4[0-9][0-9]{3})$"
  expresion_regular_fecha_num_excel <- "^(3[5-9][0-9]{3}|4[0-9][0-9]{3})$"
  # Determinamos las coincidencias en la subtabla
  coincidencias <-
    apply(
      subtabla, 2,
      function(fila) {
        (grepl(expresion_regular_anio, fila) & grepl(expresion_regular_mes, fila)) |
          grepl(expresion_regular_fecha_num_excel, fila)
      })
  # Identificamos los indices de las entradas con coincidencias
  indices_celda <- data.frame(which(coincidencias, arr.ind = TRUE))
  
  # Exigimos que haya al menos un resultado
  if ( length(indices_celda) > 0 ) {
    # Especificamos la primera coincidencia
    contenido_celda <- subtabla[indices_celda$row[1], indices_celda$col[1]]
    # Establecemos la condición para cuando el texto leído correspondeo a fecha en formato numérico de excel no
    if ( grepl(expresion_regular_fecha_num_excel, contenido_celda) ) {
      # Determinamos el valor de la celda buscada con la fecha de corte
      num_fecha_corte <- as.numeric(contenido_celda)
      # Determinamos la fecha de corte
      fecha_corte <- as.Date( num_fecha_corte, origin = "1899-12-30")
    } else {
      # Dividimos el texto original en sus componentes por si hubiera más de una fecha
      texto_dividido <- unlist(strsplit(as.character(contenido_celda), " "))
      
      grep(expresion_regular_anio, texto_dividido)
      grep(expresion_regular_mes, texto_dividido)
      
      # Determinamos las fechas almacenadas con el paquete parsedate y manipulamos para obtener un solo vector de fechas
      fechas_reconocidas <- lapply(texto_dividido, function(componente) as.Date(parsedate::parse_date(componente)))
      fechas_reconocidas <- fechas_reconocidas[[1]]
      fechas_reconocidas <- fechas_reconocidas[!is.na(fechas_reconocidas)]
      # Determinamos el año
      anio <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%Y")))[1]
      # Determinamos el mes
      mes <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%m")))[1]
      
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
        cat("\nNo se pudo determinar el año.\n")
        fecha_corte <- NA
      }
    }
    
  } else if (grepl("[0-9]{4}[-/][0-9]{2}[-/][0-9]{2}$", contenido_celda) | grepl("[0-9]{2}[-/][0-9]{2}[-/][0-9]{4}$", contenido_celda)) {
    
    fecha_corte <- parsedate::parse_date(contenido_celda)
    
  } else {
    
    cat("\nNo se pudo encontrar ningna fecha.\n")
    fecha_corte <- NA
    
  }
  return(as.Date(fecha_corte))
}

lista_pruebaBAL <- list()
lista_pruebaBAL <- head(lista_hojasBAL, 122) 

lista_fechasBAL <-
  sapply(
    lista_pruebaBAL,
    function(tabla) {
      fecha <- analisisDifusoNLPFechaCorte(tabla)
      barraProgreso(lista_pruebaBAL)
      return(fecha)
    })

fechasBAL  <- as.data.frame( as.Date(lista_fechasBAL, origin = "1970-01-01") )

tabla <- lista_hojasPYG[[1]]
analisisDifusoNLPFechaCorte(tabla)
analisisDifusoNLPFechaCorte(lista_hojasPYG[[26]])


lista_prueba <- list()
lista_prueba <- head(lista_hojasPYG, 40) 

lista_fechasPYG <-
  sapply(
    lista_prueba,
    function(tabla) {
      analisisDifusoNLPFechaCorte(tabla)
      barraProgreso(lista_prueba)
      })

fechasPYG  <- as.data.frame( as.Date(lista_fechasPYG, origin = "1970-01-01") )



#prueba_anio <- grepl("[0-9]{4}", texto)
prueba_anio <- grepl("[[:digit:]]{4}", subtabla)
# Verificamos si la expresión incluye texto únicamente con símbolos de letras, que pueda coincidir con el mes
prueba_texto <- grepl("[[:alpha:]]", texto)

lapply(tabla[1:6,], parse_date)