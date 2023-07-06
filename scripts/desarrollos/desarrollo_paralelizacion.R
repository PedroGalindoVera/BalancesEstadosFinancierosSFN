# las pruebas indican que la parelización relentiza la sustitución de caracteres
# en todos los casos la parelizacion aun usando todos los procesadores es mucho mas lento
# foreach mostro ser capaz de forzar todos los nucleos y el procesador, pero no mejor los tiempos de ejecucion de ninguna de las funciones porbadas
# foreach mostro recomponer adecuadamente los datos luego de realizar el proseso
# parallel distorcina el orden de las entradas

CUENTA <- tabla_concatenada %>% pull(CUENTA)
texto_vector <- CUENTA %>% sample(.,500000)

prueba <- function(texto_vector) {
  texto_modificado <-
    texto_vector %>%
    chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>% #Remplaza tildes
    gsub(" {2,}", " ", .) %>% #Elimina espaciados múltiples
    gsub("^\\s+", "", .) %>% #Elimina espaciados al inicio de la cadena
    gsub("\\s+$", "", .) #Elimina espaciados al final de la cadena
  return(texto_modificado)
}

pruebaParalela <- function(texto_vector) {
  texto_modificado <-
    parallel::mclapply(texto_vector, function(texto) {
      texto %>%
      chartr("[ÁÉÍÓÚ.]", "[AEIOU ]", .) %>% #Remplaza tildes
      gsub(" {2,}", " ", .) %>% #Elimina espaciados múltiples
      gsub("^\\s+", "", .) %>% #Elimina espaciados al inicio de la cadena
      gsub("\\s+$", "", .) #Elimina espaciados al final de la cadena
  })
  return(texto_modificado)
}

library(foreach)
library(doParallel)
library(dplyr)

pruebaParalelaForeach <- function(texto_vector) {
  registerDoParallel()
  patron <- c("Á","É","Í","Ó","Ú")
  remplazo <- C("A","E","I","O","U")
  texto_modificado <- foreach(k = 1:5, .combine = c) %dopar% {
      gsub(patron[k], remplazo[k], texto_vector)
  }
  return(texto_modificado)
}

system.time(prueba(texto_vector))
system.time(pruebaParalela(texto_vector))
system.time(pruebaParalelaForeach(texto_vector))




correcionCaracteresVectorizadaParalelaForeach <- function(texto_vector) {
  requerirPaquetes("parallel", "stats", "stringr", "foreach", "doParallel")
  
  # Registrar un backend de paralelización
  registerDoParallel()
  
  analisis_caracteres <- analisisCaracteresIncorrectos(texto_vector)$caracter
  if (nrow(analisis_caracteres) > 0) {
    cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
    print(analisis_caracteres)
    caracter_incorrecto <- c("  ", analisis_caracteres$original)
    caracter_correcto <- c(" ", analisis_caracteres$identificado)
    correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
    cat("\n\033[1;32mCorreción de caracteres vectorizada...\033[0m\n")
    
    # Aplicar la función str_replace_all a cada elemento de texto_vector en paralelo
    texto_corregido <- foreach(x = texto_vector, .combine = c) %dopar% {
      stringr::str_replace_all(x, correcciones)
    }
    
    return(texto_corregido)
  } else {
    cat("\n\033[1;32mNo se encontró caracteres por corregir.\033[0m\n")
  }
}
correcionCaracteresVectorizadaParalelaParallel <- function(texto_vector) {
  requerirPaquetes("parallel", "stats", "stringr")
  
  analisis_caracteres <- analisisCaracteresIncorrectos(texto_vector)$caracter
  if (nrow(analisis_caracteres) > 0) {
    cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
    print(analisis_caracteres)
    caracter_incorrecto <- c("  ", analisis_caracteres$original)
    caracter_correcto <- c(" ", analisis_caracteres$identificado)
    correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
    cat("\n\033[1;32mCorreción de caracteres vectorizada...\033[0m\n")
    
    # Aplicar la función str_replace_all a cada elemento de texto_vector en paralelo
    texto_corregido <- mclapply(texto_vector, function(x) {
      stringr::str_replace_all(x, correcciones)
    })
    
    return(texto_corregido)
  } else {
    cat("\n\033[1;32mNo se encontró caracteres por corregir.\033[0m\n")
  }
}


texto_modificado <- correcionCaracteresVectorizadaParalela(texto_vector)

data.frame(texto_vector,texto_modificado) %>%
  mutate(texto_vector != texto_modificado) %>% distinct() %>% View()

texto_vector <- CUENTA %>% sample(.,500000)
system.time(correcionCaracteresVectorizada(texto_vector))
# user  system elapsed 
# 3.83    0.11    4.11
system.time(correcionCaracteresVectorizadaParalelaForeach(texto_vector))
# user  system elapsed 
# 263.84   31.89  519.72 
system.time(correcionCaracteresVectorizadaParalelaParallel(texto_vector))
# user  system elapsed 
# 456.57  115.72  589.78 


texto_vector <- CUENTA #%>% sample(.,1000000)
# Habilitar la compilación JIT
compiler::enableJIT(3)
system.time(prueba(texto_vector))
# user  system elapsed 
# 140.16    0.33  142.55 
compiler::enableJIT(0)
system.time(prueba(texto_vector))
# user  system elapsed 
# 141.42    0.17  142.53 



