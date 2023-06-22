vectorTexto2palabras <- function(vector_texto, separadores, expresion_regular) {
  texto_unico <- unique(vector_texto)
  texto_separado <- unlist(strsplit(texto_unico, separadores))
  palabras <- sort(unique(texto_separado))
  incide_ocurrencia <- grepl(expresion_regular,palabras)
  return(palabras[incide_ocurrencia])
}

palabrasCorrectas <- function(vector_texto) {
  expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0]+$"
  separadores <- "[ .;/-]"
  vectorTexto2palabras(vector_texto, separadores, expresion_regular)
}

palabrasExtranias <- function(vector_texto, patron_extranio) {
  expresion_regular <- patron_extranio
  separadores <- "[ .;/-]"
  vectorTexto2palabras(vector_texto, separadores, patron_extranio)
}

caracteresExtranios <- function(vector_texto, certidumbre = NULL) {
  #expresion_regular <- "[^[:alpha:][:digit:] .,;/\\(\\)-]"
  expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)-]{1,}"
  tamanio_total <- length(vector_texto)
  if ( is.null(certidumbre) ) certidumbre = 0.05
  tamanio_muestra <- max(min(1000, tamanio_total), ceiling(certidumbre*tamanio_total))
  indices_muestra <- sample(1:tamanio_total, tamanio_muestra)
  vector_muestra <- vector_texto[indices_muestra]
  ocurrencias <- gregexpr(expresion_regular, vector_muestra)
  extraccion <- regmatches(vector_muestra, ocurrencias)
  return(sort(unique(unlist(extraccion))))
}

caracteresDiferentes <- function(cadena1, cadena2) {
  # el problema son las letras repetidas
  caracteres1 <- unique(unlist(strsplit(cadena1, "")))
  caracteres2 <- unique(unlist(strsplit(cadena2, "")))
  caracteres_diferentes1 <- setdiff(caracteres1, caracteres2)
  caracteres_diferentes2 <- setdiff(caracteres2, caracteres1)
  list(
    paste(caracteres_diferentes1, collapse = ""),
    paste(caracteres_diferentes2, collapse = "")
  )
}

identificarCaracteresExtranios <- function(vector_texto, certidumbre = NULL) {
  palabras_correctas <- palabrasCorrectas(vector_texto)
  caracteres_extranios <- caracteresExtranios(vector_texto, certidumbre)
  caracteres_extranios <- caracteres_extranios[caracteres_extranios != "\""]
  #caracteres_extranios <- caracteres_extranios[caracteres_extranios !%in% c("\"","-")]
  expresion <- data.frame()
  lista_palabras <- list()
  lista_distancias <- list()
  for (k in seq_along(caracteres_extranios)) {
    caracter_extranio <- caracteres_extranios[k]
    palabras_extranias <- palabrasExtranias(vector_texto,caracter_extranio)
    distancia <- stringdist::stringsimmatrix(palabras_correctas,
                                             palabras_extranias,
                                             method = "jw")
    rownames(distancia) <- palabras_correctas
    colnames(distancia) <- palabras_extranias
    # distancia[distancia==0], if distancia > 0
    indice_fila <- apply(distancia, 2, which.max)
    palabra <- data.frame(
      original = palabras_extranias,
      similar = palabras_correctas[indice_fila])
    caracteres_similares <-
      sapply(seq_len(nrow(palabra)),
             function(k) caracteresDiferentes(palabra$original[k], palabra$similar[k])[[2]])
    tabla_frecuencias <- table(caracteres_similares)
    caracter_similar_mas_repetido <- names(which.max(tabla_frecuencias))
    expresion[k,c("original","identificada")] <-
      c(caracter_extranio, caracter_similar_mas_repetido)
    nombre_abributo <- paste(caracter_extranio,"~",caracter_similar_mas_repetido)
    lista_palabras[[nombre_abributo]] <- palabra
    lista_distancias[[nombre_abributo]] <- distancia
  }
  #attr(expresion,lista_atributos)
  return(list(expresion = expresion,
              lista_palabras = lista_palabras,
              lista_distancias = lista_distancias))
}

textoConCaracteresExtranios <- function(vector_texto) {
  texto_unico <- sort(unique(vector_texto))
  #expresion_regular <- "[^[:alpha:][:digit:] .,;/\\(\\)-]"
  expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)-]"
  prueba <- grepl(expresion_regular,texto_unico)
  return(texto_unico[prueba])
}

correcionCaracteres <- function(vector_texto) {
  prueba_caracteres_extranio <- length(textoConCaracteresExtranios(vector_texto)) > 0
  if ( prueba_caracteres_extranio ) {
    correciones_RAZON_SOCIAL <- c("Ã‘" = "Ñ", "Ã‰" = "É", "Ã\u008d" = "Í", "Ã“" = "Ó")
    RAZON_SOCIAL_corregida <- stringr::str_replace_all(vector_texto, correciones_RAZON_SOCIAL)
    return(RAZON_SOCIAL_corregida)
  }
}

# archivo ----
ruta_directorio <- "data/Fuente/SEPS/Bases de Datos/Estados Financieros"
rutas <- list.files(ruta_directorio, recursive = TRUE, full.names = TRUE)
ruta <- rutas[6]
#ruta <- "data/Base de Datos/SEPS Bases de Datos 2023-03-31.csv"
tabla <- readr::read_delim(ruta, guess_max = 1000)
#tabla <- SEPS

# pruebas ----
vector_texto <- tabla$`RAZON_SOCIAL`
textoConCaracteresExtranios(vector_texto)
palabras_correctas <- palabrasCorrectas(vector_texto)
caracteres_extranios <- caracteresExtranios(vector_texto, certidumbre = 0.001)
caracteres_extranios <- caracteres_extranios[caracteres_extranios != "\""]

expresion <- identificarCaracteresExtranios(vector_texto)

ident <- identificarCaracteresExtranios(tabla$`RAZON_SOCIAL`, certidumbre = 0.0001)
identCuenta <- identificarCaracteresExtranios(tabla$CUENTA, certidumbre = 0.01)

identificarCaracteresExtranios(c(tabla$`RAZON_SOCIAL`,tabla$CUENTA))
