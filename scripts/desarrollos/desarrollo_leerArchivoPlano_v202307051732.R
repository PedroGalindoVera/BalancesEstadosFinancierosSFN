data <- readr::read_delim(archivo,delim = "\t")

data[1,] %>% str_split(pattern = "\t") %>% unlist()

library(tidyr)
data_ <- data %>%
  separate(`FECHA DE CORTE\tSEGMENTO\tRUC\tRAZON SOCIAL\tCUENTA\tDESCRIPCION CUENTA\tSALDO`,
           into = c("FECHA DE CORTE", "SEGMENTO", "RUC", "RAZON SOCIAL", "CUENTA", "DESCRIPCION CUENTA", "SALDO"),
           sep = "\t")

lista_tablas_SEPS[[nombre_tabla]] <-
  tryCatch({
     readr::read_delim(archivo, guess_max = 1000)
  }, error = function(e) {
    # Manejar el error aquí
    message("Ocurrió un error al leer el archivo: ", e)
    
    library(tidyr)
    readr::read_delim(archivo, delim = "\t") %>%
      separate(`FECHA DE CORTE\tSEGMENTO\tRUC\tRAZON SOCIAL\tCUENTA\tDESCRIPCION CUENTA\tSALDO`,
               into = c("FECHA DE CORTE", "SEGMENTO", "RUC", "RAZON SOCIAL", "CUENTA", "DESCRIPCION CUENTA", "SALDO"),
               sep = "\t")
  })

leerArchivoPlano <- function(archivo) {
  requerirPaquetes("readr","tidyr")
  tryCatch({
    readr::read_delim(archivo, guess_max = 1000)
  }, error = function(e) {
    tabla_mal_leida <- readr::read_delim(archivo, delim = "\t")
    nombre_columnas_sin_separar <- names(tabla_mal_leida)
    nombre_columnas <- str_split(nombre_columnas_sin_separar,"\t") %>% unlist()
    tabla <-
      tabla_mal_leida %>%
      tidyr::separate(nombre_columnas_sin_separar, into = nombre_columnas, sep = "\t") %>%
      mutate(SALDO = gsub(",",".",SALDO) %>% as.numeric())
  })
}


