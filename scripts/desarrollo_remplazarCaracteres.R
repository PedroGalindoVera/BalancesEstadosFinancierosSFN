library(dplyr)
rm(a,b)
a <- lista_SEPS[[8]] %>% sample_n(100000) %>% pull(CUENTA) 

SEPS <- lista_SEPS %>% bind_rows()
a <- SEPS %>% pull(CUENTA) 
b <- grep("[áéíóúÁÉÍÓÚ]",a)
c <- a[b]
d <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", a)

CUENTA <- SEPS %>% pull(CUENTA) 
caracter <- grep("[üÜ]", CUENTA)

expresion_regular <-"[^a-záéíóúñA-ZÁÉÍÓÚÑ0-9 (),–/]"
expresion_regular <-
  paste0("[^",
         "(?#minusculas)a-z",
         "(?#minusculas tildadas)áéíóúñ",
         "(?#mayusculas)A-Z",
         "(?#mayusculas tildadas)ÁÉÍÓÚÑ",
         "(?#numeros)0-9",
         "(?#separadores) (),–/",
         "]")
indices_caracteres_irregulares <- grep(expresion_regular, CUENTA)
unique(CUENTA[indices_caracteres_irregulares])

system.time(chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", CUENTA))

system.time(gsub("ÁÉÍÓÚáéíóú", "AEIOUaeiou", CUENTA))

system.time(str_replace_all(CUENTA, c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u")))

analisis <- analisisCaracteresIncorrectos(CUENTA)$caracter


expresion_regular_errores <- paste0("[",paste0(analisis$original,collapse = ""),"]")
expresion_regular_correcciones <- paste0("[",paste0(analisis$identificado,collapse = ""),"]")
chartr(expresion_regular_errores,expresion_regular_correcciones,sample(CUENTA,100000))


anterior_vector <- analisis$original
nuevo_vector <- analisis$identificado
vector_texto <- sample(CUENTA,1000000)

remplazarCaracteres <- function(anterior_vector, nuevo_vector, vector_texto) {
  # En prueba unitarias "remplazoCaracteres" mostro ser mas lento que "stringr::str_remove_all"
  tamanio_anterior_vector <- length(anterior_vector)
  tamanio_nuevo_vector <- length(nuevo_vector)
  texto_corregido <- vector_texto
  if ( tamanio_anterior_vector == tamanio_nuevo_vector ) {
    for ( k in seq_len(tamanio_anterior_vector) ) {
      texto_corregido <- gsub(anterior_vector[k], nuevo_vector[k], texto_corregido)
    }
  }
  return(texto_corregido)
}

vector_texto <- sample(CUENTA,2000000)
system.time( remplazarCaracteres(anterior_vector, nuevo_vector, vector_texto) )
system.time( str_remove_all(vector_texto, setNames(nuevo_vector,anterior_vector)) )

CUENTA_corregida <-
  sapply(seq_len(nrow(analisis)), function(k)
    gsub(analisis$original[k], analisis$identificado[k], CUENTA))

system.time(gsub(expresion_regular_errores,expresion_regular_correcciones,sample(CUENTA,100000)))

indices_caracteres_irregulares <- grep(expresion_regular, CUENTA_corregida)
unique(CUENTA_corregida[indices_caracteres_irregulares])

