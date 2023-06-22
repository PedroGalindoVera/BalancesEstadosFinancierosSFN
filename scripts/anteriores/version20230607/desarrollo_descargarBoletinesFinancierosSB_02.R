library(dplyr)
library(rvest)
library(stringr)

pagina <- read_html(ruta_archivo_html)

informacion_archivos <-
  pagina %>% 
  html_nodes(".entry.file") %>% 
  purrr::map_df(~{
    data.frame(
      ids_archivo = .x %>% html_attr("data-id"),
      nombres_archivo = .x %>% html_node(".entry-info-name span") %>% html_text(),
      dias = .x %>% html_node(".entry-info-modified-date") %>% html_text(),
      fechas = .x %>% html_node(".description-file-info") %>% html_text() %>%
        str_extract("\\d+\\s[a-zA-Z]+,\\s\\d{4}\\s\\d+:\\d+\\s[ap]m"),
      tamanos_archivo = .x %>% html_node(".entry-info-size") %>% html_text() %>%
        parse_number()
    )
  })

informacion_enlaces <-
  data.frame(
    enlaces_descarga =
      pagina %>% html_nodes(".entry.file .entry_action_download") %>% html_attr("href")
  ) %>%
  mutate(
    ids_enlaces = stringr::str_extract(enlaces_descarga, "(?<=id=)[^&]+")
  ) %>%
  inner_join(informacion_archivos, by = c("ids_enlaces" = "ids_archivo"))

informacion_enlases_selecionados <-
  informacion_enlaces %>%
  distinct(ids_enlaces, .keep_all = TRUE)



