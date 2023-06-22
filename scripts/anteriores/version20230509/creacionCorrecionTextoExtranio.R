textoConCaracteresExtranios <- function(vector_texto) {
  texto_unico <- sort(unique(vector_texto))
  expresion_regular <- "[^a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]"
  prueba <- grepl(expresion_regular,texto_unico)
  return(texto_unico[prueba])
}

textoConCaracteresCorrectos <- function(vector_texto) {
  texto_unico <- sort(unique(vector_texto))
  expresion_regular <- "^[a-zñáéíóúüA-ZÑÁÉÍÓÚÜ0-9 .,;/\\(\\)\"–-]+$"
  prueba <- grepl(expresion_regular,texto_unico)
  return(texto_unico[prueba])
}

identificarTextoExtranio <- function(texto) {
  texto_unico <- sort(unique(texto))
  texto_con_caracteres_extranios <- textoConCaracteresExtranios(texto_unico)
  texto_con_caracteres_correctos <- textoConCaracteresCorrectos(texto_unico)
  distancia <-
    stringdist::stringsimmatrix(
      texto_con_caracteres_correctos, texto_con_caracteres_extranios)
  rownames(distancia) <- texto_con_caracteres_correctos
  colnames(distancia) <- texto_con_caracteres_extranios
  indice_simililar <- apply(distancia, 2, which.max)
  texto <- data.frame(
    extranio = texto_con_caracteres_extranios,
    similar = texto_con_caracteres_correctos[indice_simililar])
  return(texto)
}


tabla <- SEPS
texto <- tabla$CUENTA
identificacion <- identificarTextoExtranio(texto)
# crear un vector con nombre a partir de dos vectores, uno con los nombres y otro con los valores
correcciones <-
  stats::setNames(identificacion$extranio, identificacion$similar)
texto_corregido <-
  stringr::str_replace_all(head(texto, 10000000), correcciones )

system.time(stringr::str_replace_all(head(texto, 10000000), correcciones ))



