palabra_incorrecta <- "CAÃ‘AR"
palabra_correcta <- "CAÑAR"

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
  if ( any(discrepancias_frecuencia_caracteres) ) {
    caracter_correcto <- caracteres_comunes[discrepancias_frecuencia_caracteres]
  }
  
  return(list(incorrecto = caracter_incorrecto, correcto = caracter_correcto))
  
}

caracterDiscrepante("BAÃ‘OS", "BAÑOS")
caracterDiscrepante("BAÑOS", "BAÃ‘OS")
caracterDiscrepante("CAÃ‘AR", "CAÑAR")

caracterDiscrepante("COLOCACION", "COLOCACIÃ“N")
caracterDiscrepante("COLOCACIÃ“N", "COLOCACION")

caracterDiscrepante("COLÃ‘CACIÃ“N", "COLUCACION")
caracterDiscrepante("COLUCACION","COLÃ‘CACIÃ“N")
caracterDiscrepante("COLON", "COLON")

vector_texto <- SEPS$RAZON_SOCIAL
certidumbre <- 0.001

analisisCaracteresIncorrectos <- function(vector_texto, certidumbre = NULL) {
  palabras_correctas <- palabrasCorrectas(vector_texto)
  caracteres_incorrectos <- caracteresExtranios(vector_texto, certidumbre)
  caracter <- data.frame()
  lista_palabras <- list()
  lista_distancias <- list()
  for (k in seq_along(caracteres_incorrectos)) {
    caracter_incorrecto <- caracteres_incorrectos[k]
    palabras_incorrectas <- palabrasExtranias(vector_texto, caracter_incorrecto)
    distancia <-
      stringdist::stringsimmatrix(
        palabras_correctas, palabras_incorrectas, method = "jw")
    rownames(distancia) <- palabras_correctas
    colnames(distancia) <- palabras_incorrectas
    if ( any(distancia > 0) ) {
      palabras_maximal <- apply(distancia, 2, which.max)
      palabra <- data.frame(
        original = palabras_incorrectas,
        similar = palabras_correctas[palabras_maximal])
      caracteres_identificados <-
        sapply(seq_len(nrow(palabra)),
               function(k) caracterDiscrepante(
                 palabra$original[k], palabra$similar[k])$correcto)
      tabla_frecuencias <- table(caracteres_identificados)
      caracter_identificado <- names(which.max(tabla_frecuencias))
      caracter[k, c("original","identificada")] <-
        stats::na.omit(c(caracter_incorrecto, caracter_identificado))
      nombre_abributo <-
        paste0("\"",caracter_incorrecto,"\"~\"",caracter_identificado,"\"")
      lista_palabras[[nombre_abributo]] <- palabra
      lista_distancias[[nombre_abributo]] <- distancia
    }
  }
  return(
    list(caracter = data.frame(caracter, row.names = NULL),
         lista_palabras = lista_palabras,
         lista_distancias = lista_distancias))
}

t <- analisisCaracteresIncorrectos(SEPS$CUENTA, 0.0001)
