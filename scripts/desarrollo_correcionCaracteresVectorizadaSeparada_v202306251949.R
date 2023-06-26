correcionCaracteresVectorizadaSeparada <- function(texto_vector) {
  requerirPaquetes("stats", "stringr")
  
  analisis_caracteres <- analisisCaracteresIncorrectos(texto_vector)$caracter
  if (nrow(analisis_caracteres) > 0) {
    cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
    print(analisis_caracteres)
    caracter_incorrecto <- analisis_caracteres$original
    caracter_correcto <- analisis_caracteres$identificado
    correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
    cat("\n\033[1;32mCorreción de caracteres vectorizada...\033[0m\n")
    
    indices <- seq_along(texto_vector)
    partes <- split(indices, cut(indices, breaks = 100, labels = FALSE))
    texto_corregido <- character()
    barraProgresoReinicio()
    for (parte in partes) {
      texto_corregido[parte] <-
        stringr::str_replace_all(texto_vector[parte], correcciones)
      #barraProgreso(partes)
      barraProgreso2(partes)
    }
    
    return(texto_corregido)
  } else {
    cat("\n\033[1;32mNo se encontró caracteres por corregir.\033[0m\n")
  }
}

texto_corregido <- correcionCaracteresVectorizadaSeparada(texto_vector)

data.frame(texto_vector,texto_corregido,texto_vector!=texto_corregido) %>% View()


CUENTA <- tabla_concatenada %>% pull(CUENTA)
texto_vector <- CUENTA %>% sample(.,1000000)
system.time(correcionCaracteresVectorizada(texto_vector))
system.time(correcionCaracteresVectorizadaSeparada(texto_vector))