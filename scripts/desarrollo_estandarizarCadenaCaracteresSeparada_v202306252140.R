estandarizarCadenaCaracteresSeparada <- function(texto_vector, ver_control_cambios = FALSE) {
  requerirPaquetes("dplyr","stringdist")
  tabla_control_cambios <- function(texto_vector){
    texto_vector_unico <- unique(texto_vector)
    texto_modificado_unico <-
      estandarizarCadenaCaracteresSeparada(texto_vector_unico)
    control_cambios <-
      data.frame(
        original = texto_vector_unico,
        modificado = texto_modificado_unico) %>%
      dplyr::mutate(
        similitud = stringdist::stringsim(original,modificado),
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
  estandarizar <- function(texto_vector) {
    texto_modificado <-
      texto_vector %>%
      chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>% #Remplaza tildes
      gsub(" {2,}", " ", .) %>% #Elimina espaciados múltiples
      gsub("^\\s+", "", .) %>% #Elimina espaciados al inicio de la cadena
      gsub("\\s+$", "", .) #Elimina espaciados al final de la cadena
    return(texto_modificado)
  }
  if ( !is.character(texto_vector) ){texto_vector <- as.character(texto_vector)}
  if ( isTRUE(ver_control_cambios) ) {
    return(tabla_control_cambios(texto_vector))
  }
  cat("\n\033[1;32mEstandarizando caracteres comunes...\033[0m\n")
  
  indices <- seq_along(texto_vector)
  partes <- split(indices, cut(indices, breaks = 100, labels = FALSE))
  texto_modificado <- character()
  barraProgresoReinicio()
  for (parte in partes) {
    texto_modificado[parte] <- estandarizar(texto_vector[parte])
      # texto_vector[parte] %>%
      # chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>% #Remplaza tildes
      # gsub(" {2,}", " ", .) %>% #Elimina espaciados múltiples
      # gsub("^\\s+", "", .) %>% #Elimina espaciados al inicio de la cadena
      # gsub("\\s+$", "", .) #Elimina espaciados al final de la cadena
    barraProgreso2(partes)
  }
  return(texto_modificado)
}

CUENTA <- tabla_concatenada %>% pull(CUENTA)
texto_vector <- CUENTA

A <- estandarizarCadenaCaracteres(texto_vector, ver_control_cambios = TRUE)

B <- estandarizarCadenaCaracteresSeparada(texto_vector, ver_control_cambios = TRUE)

CUENTA_estandarizada <- estandarizarCadenaCaracteresSeparada(texto_vector)


data.frame(CUENTA,CUENTA_estandarizada,
           stringdist::stringsim(CUENTA,CUENTA_estandarizada)) %>% View()
