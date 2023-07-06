caracteresUnicosCadena <- function(texto_vector){
  if ( !is.character(texto_vector) ){texto_vector <- as.character(texto_vector)}
  texto_vector_unico <- unique(texto_vector)
  caracteres_unicos <- paste(texto_vector_unico, collapse = " ") %>%
    str_split(.,"") %>% unlist() %>% unique() %>% sort()
  return(caracteres_unicos)
}

estandarizarCadenaCaracteres <- function(texto_vector, ver_control_cambios = FALSE) {
  requerirPaquetes("dplyr")
  tabla_control_cambios <- function(texto_vector_unico){
    texto_modificado_unico <-
      estandarizarCadenaCaracteres(texto_vector_unico)
    control_cambios <-
      data.frame(
        original = texto_vector_unico,
        modificado = texto_modificado_unico) %>%
      dplyr::mutate(
        cambios = original != modificado,
        "ÁÉÍÓÚáéíóú" = grepl("[ÁÉÍÓÚáéíóú]", original),
        #separadores = grepl("[\"\\./,;–-]", original),
        " ." = grepl("\\.", original),
        #"," = grepl(",", original),
        #";" = grepl(";", original),
        "–" = grepl("–", original),
        #"-" = grepl("-", original),
        #"\"" = grepl("\"", original),
        #"/" = grepl("/", original),
        "mas_de_1_espacio" = grepl(" {2,}", original),
        "espacio_inicial" = grepl("^\\s+", original),
        "espacio_final" = grepl("\\s+$", original)
      )
    return(control_cambios)
  }
  if ( !is.character(texto_vector) ){texto_vector <- as.character(texto_vector)}
  if ( isTRUE(ver_control_cambios) ) {
    return(tabla_control_cambios(texto_vector))
  }
  texto_modificado <-
    texto_vector %>%
    chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>% #Remplaza tildes
    gsub(" {2,}", " ", .) %>% #Elimina espaciados múltiples
    gsub("^\\s+", "", .) %>% #Elimina espaciados al inicio de la cadena
    gsub("\\s+$", "", .) #Elimina espaciados al final de la cadena
  return(texto_modificado)
}

CUENTA <- SEPS %>% pull(CUENTA)
CUENTA_muestra <- CUENTA %>% sample(.,10000000)
CUENTA_unico <- CUENTA %>% unique()

CUENTA_muestra_modificado <- estandarizarCadenaCaracteres(CUENTA)

RAZON_SOCIAL <- SEPS %>% pull(RAZON_SOCIAL)
RAZON_SOCIAL_muestra <- RAZON_SOCIAL %>% sample(.,10000000)
RAZON_SOCIAL_unico <- RAZON_SOCIAL %>% unique()

RAZON_SOCIAL_muestra_modificado <- estandarizarCadenaCaracteres(RAZON_SOCIAL)

modificacion <- estandarizarCadenaCaracteres(RAZON_SOCIAL_unico) %>%
  strsplit(.," ") %>% unlist() %>% unique() %>% sort()

control <- estandarizarCadenaCaracteres(RAZON_SOCIAL_unico, ver_control_cambios = TRUE)




