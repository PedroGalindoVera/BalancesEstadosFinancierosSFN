nombres_archivos <- basename(list.files("data/Descargas/SB",recursive = TRUE))

expresion_regular <- paste0(c(
  #"(?<= )[A-Z]{3,10} [0-9]{4}(?=\\.zip)",
  #"(?<=_)[A-Z]{3,4}_[0-9]{2}(?=\\.zip)",
  #"(?<=-)[A-Z]{3}-[0-9]{2}(?=\\.zip)",
  "(?<=[_-])[A-Z]{3,10}[ _-][0-9]{2}(?=\\.zip)",
  "(?<=_)[0-9]{4}(?=\\.zip)",
  "(?<=)[0-9]{4}(?=\\.zip)"
  ),
  collapse = "|"
)

library(dplyr)
library(stringr)

fecha_nombre_archivo <-
  nombres_archivos %>%
  stringr::str_extract(expresion_regular) %>%
  stringr::str_replace_all("[-_]", " ")

data.frame(nombres_archivos,fecha_nombre_archivo, row.names = NULL)

as.Date(
  fecha_nombre_archivo,
  format = c("%b %y", "%Y")
)
