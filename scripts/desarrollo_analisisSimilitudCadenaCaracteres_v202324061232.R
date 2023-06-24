analisisSimilitudCadenaCaracteres <- function(texto_vector, metodo = "lv",
    tokenizar = FALSE, min = NA, max = NA) {
  requerirPaquetes("stringdist")
  if ( isTRUE(tokenizar) ) {texto_vector <- unlist(strsplit(texto_vector, " "))}
  cadena_caracteres <- sort(unique(texto_vector))
  similitud <-
    stringdist::stringsimmatrix(cadena_caracteres, cadena_caracteres, metodo)
  rownames(similitud) <- cadena_caracteres
  colnames(similitud) <- cadena_caracteres
  similitud_unica <- unique(sort(similitud, decreasing = TRUE))
  min <- ifelse(is.na(min), min(similitud_unica), min)
  max <- ifelse(is.na(max), 1, max)
  similitud_seleccion <-
    similitud_unica[similitud_unica >= min & similitud_unica <= max]
  similar <- data.frame()
  barraProgresoReinicio()
  for ( similitud_iteracion in similitud_seleccion ) {
    indice <-
      as.data.frame(which(similitud == similitud_iteracion, arr.ind = TRUE))
    similar_iteracion <-
      data.frame(
        original = rownames(similitud)[indice$row],
        similar = colnames(similitud)[indice$col],
        similitud = similitud_iteracion
      )
    similar <- rbind(similar, similar_iteracion)
    barraProgreso(similitud_seleccion)
  }
  filas_duplicadas <-
    duplicated(t(apply(similar[, c("original", "similar")], 1, sort)))
  similar <- similar[!filas_duplicadas, ]
  return(similar)
}

analisisSimilitudCadenaCaracteres(c("hola","helo","hilo","halo","hola","eco"))

RAZON_SOCIAL <- SEPS %>% pull(RAZON_SOCIAL)
RAZON_SOCIAL_unico <- RAZON_SOCIAL %>% unique()

A <- analisisSimilitudCadenaCaracteres(RAZON_SOCIAL_unico)

modificacion <- estandarizarCadenaCaracteres(RAZON_SOCIAL_unico)

fraces <- analisisSimilitudCadenaCaracteres(
  modificacion, metodo = "dl", min = 0.9, max = 0.9999)

palabras <- analisisSimilitudCadenaCaracteres(
  modificacion, metodo = "jw", tokenizar = TRUE, min = 0.8, max = 0.9999)
