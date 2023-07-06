estandarizarCadenaCaracteres <- function(texto_vector, 
    patron = NULL, remplazo = NULL, 
    ver_caracteres = FALSE, ver_control_cambios = FALSE) {
  
  requerirPaquetes("dplyr","stringr")
  
  lista_modificaciones <- function(patron = NULL, remplazo = NULL) {
    patron_defecto  <- c("Á", "É", "Í", "Ó", "Ú", "á", "é", "í", "ó", "ú", "\\.", "–", " {2,}", "^\\s+", "\\s+$")
    remplazo_defecto  <- c("A", "E", "I", "O", "U", "a", "e", "i", "o", "u",  "", "-", " ", "", "")
    prueba_vacio <- length(patron) > 0 & length(remplazo) > 0
    prueba_numero_elementos <- length(patron) == length(remplazo)
    if ( !prueba_vacio & prueba_numero_elementos ) {
      patron <- patron_defecto
      remplazo <- remplazo_defecto
    } else if ( prueba_vacio & prueba_numero_elementos ) {
      patron <- union(patron_defecto, patron)
      remplazo <- union(remplazo_defecto, remplazo)
    } else if ( prueba_vacio & !prueba_numero_elementos ) {
      stop("\nError: los tamaños del patron y el remplazo no coinciden.")
    }
    #seleccion = list(patron = patron, remplazo = remplazo)
    seleccion <- setNames(remplazo, patron)
    return(seleccion)
  }
  caracteres <- function(texto_vector_unico){
    caracteres_unicos <- paste(texto_vector_unico, collapse = " ") %>%
      str_split(.,"") %>% unlist() %>% unique() %>% sort()
    return(caracteres_unicos)
  }
  tabla_control_cambios <- function(texto_vector_unico, lista_modificaciones){
    texto_modificado_unico <-
      stringr::str_replace_all(texto_vector_unico, lista_modificaciones)
    control_cambios <-
      data.frame(
        original = texto_vector_unico,
        modificado = texto_modificado_unico) %>%
      dplyr::mutate(
        cambios = original != modificado,
        "ÁÉÍÓÚáéíóú" = grepl("[ÁÉÍÓÚáéíóú]", original),
        #separadores = grepl("[\"\\./,;–-]", original),
        " ." = grepl("\\.", original),
        "," = grepl(",", original),
        ";" = grepl(";", original),
        "–" = grepl("–", original),
        "-" = grepl("-", original),
        "\"" = grepl("\"", original),
        "/" = grepl("/", original),
        "mas_de_1_espacio" = grepl(" {2,}", original),
        "espacio_inicial" = grepl("^\\s+", original),
        "espacio_final" = grepl("\\s+$", original)
      )
    
    return(control_cambios)
  }
  modificar_texto <- function(texto_vector, lista_modificaciones) {
    texto_corregido <- stringr::str_replace_all(texto_vector, lista_modificaciones)
    return(texto_corregido)
  }
  
  lista_modificaciones <- lista_modificaciones(patron, remplazo)
  texto_vector <- as.character(texto_vector)
  texto_vector_unico <- unique(texto_vector)
  
  if ( isTRUE(ver_caracteres) ) {
    return(caracteres(texto_vector_unico))
  } else if ( isTRUE(ver_control_cambios) ) {
    return(tabla_control_cambios(texto_vector_unico, lista_modificaciones))
  } else {
    return(modificar_texto(texto_vector, lista_modificaciones))
  }
}

caracteresUnicosCadena <- function(texto_vector){
  texto_vector_unico <- unique(texto_vector)
  caracteres_unicos <- paste(texto_vector_unico, collapse = " ") %>%
    str_split(.,"") %>% unlist() %>% unique() %>% sort()
  return(caracteres_unicos)
}

texto_vector <- SEPS %>% pull(RAZON_SOCIAL)

A <- estandarizarCadenaCaracteres(texto_vector)
estandarizarCadenaCaracteres(A,ver_caracteres = TRUE)
control <- estandarizarCadenaCaracteres(A,ver_control_cambios = TRUE)

CUENTA <- SEPS %>% pull(CUENTA)
CUENTA_muestra <- CUENTA %>% sample(.,10000000)
CUENTA_unico <- CUENTA %>% unique()

CUENTA_modificado <-
  CUENTA %>%
  chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>%
  gsub(" {2,}", " ", .) %>%
  gsub("^\\s+", "", .) %>%
  gsub("\\s+$", "", .)

estandarizarCadenaCaracteres(CUENTA, ver_caracteres = TRUE)
estandarizarCadenaCaracteres(CUENTA_modificado, ver_caracteres = TRUE)

RAZON_SOCIAL <- SEPS %>% pull(RAZON_SOCIAL)
RAZON_SOCIAL_muestra <- RAZON_SOCIAL %>% sample(.,10000000)
RAZON_SOCIAL_unico <- RAZON_SOCIAL %>% unique()

RAZON_SOCIAL_modificado <-
  RAZON_SOCIAL %>%
  chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>%
  gsub(" {2,}", " ", .)

estandarizarCadenaCaracteres(RAZON_SOCIAL, ver_caracteres = TRUE)
estandarizarCadenaCaracteres(RAZON_SOCIAL_modificado, ver_caracteres = TRUE)


RAZON_SOCIAL_muestra_modificado <- estandarizarCadenaCaracteres(RAZON_SOCIAL_muestra)


