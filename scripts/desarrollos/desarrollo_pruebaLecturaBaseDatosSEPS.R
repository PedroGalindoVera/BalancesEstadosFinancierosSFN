k <- 1
archivo <- archivos[k]
codificacion <- readr::guess_encoding(archivo)
readr::guess_encoding(archivo, n_max = 5000)

lista_tablas_SEPS <- list()
lista_tablas_SEPS[basename(archivo)] <-
  if ( tools::file_ext(archivo) == "txt" ) {
    read.delim(archivo, fileEncoding = codificacion$encoding[1], sep = ";")
  } else if ( tools::file_ext(archivo) == "csv" ) {
    readr::read_delim(archivo, guess_max = 1000)
  }

readLines(archivo)

a <- A$RAZON_SOCIAL

l <- analisisCaracteresIncorrectos(a[1:100000])
