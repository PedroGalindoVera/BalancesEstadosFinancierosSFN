correccionCaracteresParalelizadaRazonSocialSEPS2 <- function(tabla) {
  
  # MODIFICA CON ERROR GRABE LA TABLA
  
  library(parallel)
  library(stringr)
  
  # Dividir los datos en bloques
  data_split <- split(tabla$RAZON_SOCIAL, 1:nrow(tabla) %% parallel::detectCores())
  # Definir una función para reemplazar los caracteres en cada bloque
  replace_chars_block <- function(block) {
    block <- gsub("Ã‘","Ñ",block)
    block <- gsub("Ã‰","É",block)
    block <- gsub("Ã\u008d","Í",block)
    block <- gsub("Ã“","Ó",block)
  }
  # Reemplazar los caracteres en cada bloque en paralelo
  RAZON_SOCIAL_new <- parallel::mclapply(data_split, replace_chars_block)
  # Combinar los resultados en un solo vector
  RAZON_SOCIAL_new <- unlist(RAZON_SOCIAL_new)
  # Reemplazar la columna RAZON_SOCIAL en el dataframe original
  tabla$RAZON_SOCIAL <- RAZON_SOCIAL_new
  
  return(tabla)
}

SEPS <- correccionCaracteresParalelizadaRazonSocialSEPS2(SEPS)
