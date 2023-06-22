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

ruta_directorio <- "data/Descargas/SB/PUBLICA"

nombres_comprimidos <- basename(list.files(ruta_directorio, recursive = TRUE))
nombres_comprimidos_modificacion1 <- gsub("(?<=[[:alpha:]])(?=[[:digit:]])", " ", nombres_comprimidos, perl = TRUE)
nombres_comprimidos_modificacion2 <- gsub("(?<=[[:digit:]]{4})(?=[[:digit:]]{2})", " ", nombres_comprimidos_modificacion1, perl = TRUE)
nombres_comprimidos_modificacion3 <- gsub(".zip|.xls", "", nombres_comprimidos_modificacion2, perl = TRUE)
nombres_comprimidos_modificacion4 <- gsub("(?<=[[:digit:]]{2})E", " 01", nombres_comprimidos_modificacion3, perl = TRUE)
nombres_comprimidos_modificacion5 <- gsub("BOL |BOL[_-]FIN[_-]PUB[_-]|Boletín Financiero Instituciones Públicas ", "", nombres_comprimidos_modificacion4, perl = TRUE)
nombres_comprimidos_modificacion6 <- gsub("[_-]", " 20", nombres_comprimidos_modificacion5, perl = TRUE)

nombres_traducidos <- sapply(nombres_comprimidos_modificacion6, 
                             function(nombre) {
                               traduccion <-
                                 if (grepl("[[:alpha:]]", nombre)) traductor_mes(nombre) else nombre })

preliminar <- gsub(" ", "-", nombres_traducidos, perl = TRUE)

fecha <- parsedate::parse_date(preliminar)

tabla <- data.frame(original = nombres_comprimidos, 
                    modificacion1 = nombres_comprimidos_modificacion1,
                    modificacion2 = nombres_comprimidos_modificacion2,
                    modificacion3 = nombres_comprimidos_modificacion3,
                    modificacion4 = nombres_comprimidos_modificacion4,
                    modificacion5 = nombres_comprimidos_modificacion5,
                    modificacion6 = nombres_comprimidos_modificacion6,
                    traduccion = nombres_traducidos,
                    fecha = as.Date(fecha))
tabla <- tabla[order(tabla$fecha), ]
tabla$diferencia <- c(NA,diff(tabla$fecha))

