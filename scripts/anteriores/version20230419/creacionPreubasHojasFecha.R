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





tabla <- lista_hojasBAL[[108]]

#### INNOVACION
analisisDifusoNLPFechaCorte <- function(tabla) {
  
  # Esta función procesa un texto relacionado a un la fecha de corte de los "Balances Financieros" de SB y devuelve el date más cercano a fecha de corte.
  
  # Función para transformar a formato numérico de Excel una fecha date
  formato_numerico_excel <- function(fecha) {
    # Fecha base de Excel
    fecha_base_excel <- as.Date("1899-12-30")
    return(as.numeric(difftime(as.Date(fecha), as.Date("1899-12-30"))))
  }
  
  # Empleamos la función indicePrimeraFilDecimalTabla()
  indice_fila_nombres <- indicePrimeraFilDecimalTabla(tabla)
  # Subtabla previa a los valores decimales, y ala fila de nombres de columnas, por eso se resta 2
  subtabla <- tabla[1:(indice_fila_nombres-2),]
  
  # Año actual a texto, para generar expresión regular de año, usan Sys.Date() y descomponiéndolo
  anio_num <- year(Sys.Date())
  anio_text <- strsplit(as.character(anio_num), split = "")[[1]]
  # Establecemos una expresión regular que acepta 2000 hasta el año actual
  expresion_regular_anio <- paste0("(",anio_text[1],"[",0,"-",anio_text[2],"][",0,"-",anio_text[3],"][0-9])")
  # expresion_regular_anio <- "(2[0][0-3][0-9])$" # acepta desde 2000 hasta 2039
  #expresion_regular_anio <- "[[:digit:]]{4}"
  
  # Establecemos una expresión regular
  #expresion_regular_mes <- "[[:alpha:]]"
  expresion_regular_mes <- paste0(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"), collapse = "|")
  # Establecemos una expresión regular para identificar fechas en formato numérico de Excel desde el 2000 hasta 2036
  # expresion_regular_fecha_num_excel <- "(3[5-9][0-9]{3}|4[0-9][0-9]{3})$"
  # expresion_regular_fecha_num_excel <- "^(3[5-9][0-9]{3}|4[0-9][0-9]{3})$"
  #expresion_regular_fecha_num_excel <- "^([3-4][5-9][0-9]{3}|4[0-9][0-9]{3})$"
  
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
  # Establecemos una expresión regular que acepte variantes de formato fecha
  expresion_regular_fecha_date <- "20[0-9]{2}[-/][0-1][0-9][-/][0-3][0-9]|[0-3][0-9][-/][0-1][0-9][-/]20[0-9]{2}"
  
  # Purebas correspondientes
  prueba_anio <- function(texto) grepl(expresion_regular_anio, texto, ignore.case = TRUE)
  prueba_mes <- function(texto) grepl(expresion_regular_mes, texto, ignore.case = TRUE)
  prueba_fecha_excel <- function(texto) grepl(expresion_regular_fecha_num_excel, texto, ignore.case = TRUE)
  prueba_fecha_date <- function(texto) grepl(expresion_regular_fecha_date, texto)
  
  # Determinamos las coincidencias en la subtabla
  coincidencias <-
    apply(
      subtabla, 2,
      function(fila) {
        (prueba_anio(fila) & prueba_mes(fila)) | prueba_fecha_excel(fila) | prueba_fecha_date(fila)
      })
  # Identificamos los indices de las entradas con coincidencias
  indices_celda <- data.frame(which(coincidencias, arr.ind = TRUE))
  
  # Exigimos que haya al menos un resultado
  if ( length(indices_celda) > 0 ) {
    
    # Especificamos la primera coincidencia
    contenido_celda <- as.character(subtabla[indices_celda$row[1], indices_celda$col[1]])
    
    # Paquete para procesamiento de lenguaje natural
    if (!require("parsedate")) { 
      install.packages("parsedate")
      library(parsedate)
    }
    
    if ( prueba_fecha_date(contenido_celda) ) {
      
      fecha_corte <- parsedate::parse_date(contenido_celda)
    
    # Establecemos la condición para cuando el texto leído corresponde a fecha en formato numérico de Excel no
    } else if ( prueba_fecha_excel(contenido_celda) ) {
      
      # Determinamos el valor de la celda buscada con la fecha de corte
      num_fecha_corte <- as.numeric(contenido_celda)
      # Determinamos la fecha de corte
      fecha_corte <- as.Date( num_fecha_corte, origin = "1899-12-30")
      
    } else {
      
      # Dividimos el texto original en sus componentes por si hubiera más de una fecha
      texto_dividido <- unlist(strsplit(contenido_celda, " "))
      
      
      ####
      fechas_reconocidas0 <- parsedate::parse_date(texto_dividido)
      fechas_reconocidas0[which.max(fechas_reconocidas0)]
      indices <- which(!is.na(fechas_reconocidas0))
      fechas_reconocidas0 <- fechas_reconocidas0[indices]
      ####
      
      
      # Determinamos las fechas almacenadas con el paquete parsedate y manipulamos para obtener un solo vector de fechas
      fechas_reconocidas <- sapply(texto_dividido, function(componente) as.Date(parsedate::parse_date(componente)))
      fechas_reconocidas <- as.Date(fechas_reconocidas, origin = "1970-01-01")
      # Elegimos la primera lectura de forma arbitraria
      fechas_reconocidas <- fechas_reconocidas[!is.na(fechas_reconocidas)][1]
      # Determinamos el año
      anio <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%Y")))
      
      if ( prueba_mes(contenido_celda) ) {
        
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
        # Análisis difuso de texto
        texto_buscado <- tolower(contenido_celda)
        distancias <- stringdist::stringsim(diccionario_meses$espaniol, texto_buscado, method = "lv")
        #distancias <- stringdist::stringsimmatrix(diccionario_meses$espaniol, unlist(strsplit(texto_buscado, " ")), method = "lv")
        traduccion_mes <- diccionario_meses$ingles[which.max(distancias)]
        # Determinamos el mes
        mes <- format(parsedate::parse_date(traduccion_mes), "%m")
        
      } else {
        
        # Determinamos el mes
        mes <- sort(unique(format(parsedate::parse_date(fechas_reconocidas), "%m")))
        
      }
      
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
    
  }
  
  return(as.Date(fecha_corte))
  
}


#### Nueva idea
meses <-
  data.frame(
    es = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"),
    en = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

library(stringdist)

texto_original <- "01-Enero-2013"
texto_original <- "Noviembre 2021"
patron <- meses$es
reemplazo <- meses$en

separadores <- "[-,/, ]"
palabras <- unlist(strsplit(texto_original, separadores))
similitudes <- stringsimmatrix(palabras, patron)
posiciones_max <- as.data.frame(which(similitudes == max(similitudes), arr.ind = TRUE))
palabra_similar <- palabras[posiciones_max$row]
reemplazo_similar <- reemplazo[posiciones_max$col]
texto_modificado <- gsub(palabra_similar, reemplazo_similar, texto_original)





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

fechasBAL  <- data.frame( fecha = as.Date(lista_fechasBAL, origin = "1970-01-01") ) %>%
# Verificamos que no faltan meses between(fecha,27,31)
  arrange(as.Date(fecha)) %>%
  #mutate(diferencia_dias = c(NA, diff(as.Date(fecha)))) %>%
  mutate(diferencia_meses = difftime(fecha, lag(fecha) )) %>% View()


tabla <- lista_hojasPYG[[1]]
analisisDifusoNLPFechaCorte(tabla)
analisisDifusoNLPFechaCorte(lista_hojasPYG[[26]])


lista_prueba <- list()
lista_prueba <- head(lista_hojasPYG, 122) 

lista_fechasPYG <-
  sapply(
    lista_prueba,
    function(tabla) {
      fecha <- analisisDifusoNLPFechaCorte(tabla)
      barraProgreso(lista_prueba)
      return(fecha)
      })

fechasPYG  <- data.frame( fecha = as.Date(lista_fechasPYG, origin = "1970-01-01") ) %>%
  # Verificamos que no faltan meses between(fecha,27,31)
  arrange(as.Date(fecha)) %>%
  #mutate(diferencia_dias = c(NA, diff(as.Date(fecha)))) %>%
  mutate(diferencia_meses = difftime(fecha, lag(fecha) )) #%>% View()



#prueba_anio <- grepl("[0-9]{4}", texto)
prueba_anio <- grepl("[[:digit:]]{4}", subtabla)
# Verificamos si la expresión incluye texto únicamente con símbolos de letras, que pueda coincidir con el mes
prueba_texto <- grepl("[[:alpha:]]", texto)

lapply(tabla[1:6,], parse_date)