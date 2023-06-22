textoConCaracteresIncorrectos <- function(vector_texto) {
  texto_unico <- sort(unique(vector_texto))
  expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]"
  texto_incorrecto <- grep(expresion_regular, texto_unico, value = TRUE)
  return(texto_incorrecto[prueba])
}
textoConCaracteresCorrectos <- function(vector_texto) {
  texto_unico <- sort(unique(vector_texto))
  expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+$"
  texto_correcto <- grep(expresion_regular, texto_unico, value = TRUE)
  return(texto_correcto)
}
identificarTextoCorrecto <- function(texto, caracter_incorrecto) {
  texto_unico <- sort(unique(texto))
  texto_incorrecto <- grep(caracter_incorrecto, texto_unico, value = TRUE)
  texto_correcto <- textoConCaracteresCorrectos(texto)
  distancia <- stringdist::stringsimmatrix(texto_correcto, texto_incorrecto)
  rownames(distancia) <- texto_correcto
  colnames(distancia) <- texto_incorrecto
  indice_texto_maximal <- which.max(distancia)
  texto_similar <- texto_correcto[indice_texto_maximal]
  return(texto_similar)
}

texto <- SEPS$CUENTA
caracter_incorrecto <- "\\?"

analisisCaracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
  
  # Permite identificar en un vector de texto los caracteres ajenos a la
  # escritura en español, y retornar una tabla de los reconocimientos más
  # plausibles.
  
  vectorTexto2palabras <- function(vector_texto, separadores, expresion_regular) {
    texto_unico <- unique(vector_texto)
    texto_separado <- unlist(strsplit(texto_unico, separadores))
    palabras <- sort(unique(texto_separado))
    #incide_ocurrencia <- grepl(expresion_regular,palabras)
    #return(palabras[incide_ocurrencia])
    vector_palabras <- grep(expresion_regular, palabras, value = TRUE)
    return(vector_palabras)
  }
  palabrasCorrectas <- function(vector_texto) {
    ##expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]+$"
    ##expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9\\(\\)-]+$"
    #expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9\\(\\)\"-]+$"
    #separadores <- "[ .;/-]"
    expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]+$"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  palabrasIncorrectas <- function(vector_texto, patron_incorrecto = NULL) {
    expresion_regular <-
      if ( is.null(patron_incorrecto) ) {
        ##"[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)-]"
        #"[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]" # "–" es diferente de "-"
        "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9]"
      } else {
        patron_incorrecto
      }
    #separadores <- "[ .;/-]"
    separadores <- "[ .,;/\\(\\)\"–-]"
    vectorTexto2palabras(vector_texto, separadores, expresion_regular)
  }
  caracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
    expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+"
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
    # caracteres_conjuntos <- union(caracteres_cadena1,caracteres_cadena2)
    # caracteres_diferentes <- setdiff(caracteres_conjuntos,caracteres_comunes) #
    # tail(caracteres_diferentes,1)
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
    expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+$"
    texto_correcto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_correcto)
  }
  textoConCaracteresIncorrectos <- function(vector_texto) {
    texto_unico <- sort(unique(vector_texto))
    expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]"
    texto_incorrecto <- grep(expresion_regular, texto_unico, value = TRUE)
    return(texto_incorrecto)
  }
  
  fraces_unicas <- sort(unique(vector_texto))
  palabras_correctas <- palabrasCorrectas(fraces_unicas)
  caracteres_incorrectos <- caracteresIncorrectos(fraces_unicas, certidumbre)
  caracter <- data.frame()
  lista_palabras <- list()
  lista_distancias <- list()
  for (k in seq_along(caracteres_incorrectos)) {
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

t <- analisisCaracteresIncorrectos(SEPS$CUENTA)
t <- analisisCaracteresIncorrectos(SEPS$RAZON_SOCIAL)


#----

vector_texto <- SEPS$CUENTA
fraces_unicas <- sort(unique(vector_texto))
frace_incorrecta <- textoConCaracteresIncorrectos(fraces_unicas)
frace_correcta <- textoConCaracteresCorrectos(fraces_unicas)
distancia <- stringdist::stringsimmatrix(frace_correcta, frace_incorrecta)
rownames(distancia) <- frace_correcta
colnames(distancia) <- frace_incorrecta
indice_maximal <- apply(distancia, 2, which.max)
frace_similar <- frace_correcta[indice_maximal]
frace <-
  data.frame(
    original = frace_incorrecta,
    similar = frace_similar)
identificaciones <- data.frame()
for ( k in 1:nrow(frace) ) {
  caracter_incorrecto <-
    paste(
      setdiff(
        unlist(strsplit(frace$original[k], "")),
        unlist(strsplit(frace$similar[k], ""))),
      collapse = "")
  caracter_identificado <-
    paste(
      setdiff(
        unlist(strsplit(frace$similar[k], "")),
        unlist(strsplit(frace$original[k], ""))),
      collapse = "")
  identificaciones[k, c("original","identificado")] <- 
    c(caracter_incorrecto, caracter_identificado)
}
table(identificaciones$original)
table(identificaciones$identificado)
caracter <- sort(unique(identificaciones$original))
k <- 12
table(identificaciones$identificado[identificaciones$original == caracter[k]])


