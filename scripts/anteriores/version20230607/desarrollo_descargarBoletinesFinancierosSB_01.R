library(dplyr)
library(rvest)
library(stringr)

pagina <- read_html(ruta_archivo_html)

nombres_archivo <- pagina %>% html_nodes(".entry.file .entry-info-name span") %>% html_text()
ids_archivo <- pagina %>% html_nodes(".entry.file") %>% html_attr("data-id")
fechas <- pagina %>% html_nodes(".entry.file .entry-info-modified-date") %>% html_text()
horas <-
  pagina %>% html_nodes(".entry.file .description-file-info") %>% html_text() %>%
  stringr::str_extract(., "\\d+\\s[a-zA-Z]+,\\s\\d{4}\\s\\d+:\\d+\\s[ap]m")
tamanos_archivo <- pagina %>% html_nodes(".entry.file .entry-info-size") %>% html_text()

enlaces_descarga <-pagina %>% html_nodes(".entry.file .entry_action_download") %>% html_attr("href")
ids_enlaces <- stringr::str_extract(enlaces_descarga, "(?<=id=)[^&]+")

posiciones <- match(ids_enlaces, ids_archivo)
informacion_descargas <-
  data.frame(ids_archivo[posiciones],
             nombres_archivo[posiciones],
             fechas[posiciones],
             horas[posiciones],
             tamanos_archivo[posiciones],
             ids_enlaces,
             enlaces_descarga)
