library(rvest)
html <- read_html("html/SB/Boletines de Series/Bancos Privados/2023 03 MARZO.html", encoding = "UTF-8")
download_links <- html %>% html_nodes("a") %>% html_attr("href")

nombre_archivo <- html %>% html_nodes("span") %>% html_text()

unique_ids <- unique(sapply(download_links, function(link) {
  sub(".*id=([a-zA-Z0-9]+)&.*", "\\1", link)
}))
first_download_links <- sapply(unique_ids, function(id) {
  download_links[grepl(id, download_links)][1]
})
names(first_download_links) <- nombre_archivo 

dir.create("data/Descargas/SB/Boletines de Series")
ruta_descarga <- "data/Descargas/SB/Boletines de Series/2023_03 (MARZO)"
dir.create(ruta_descarga)
k <- 0
for (link in first_download_links) {
  k <- k + 1
  nombre_temporal <- paste0(k,".zip")
  ruta_archivo <- file.path(ruta_descarga, nombre_temporal)
  download.file(link, destfile = ruta_archivo, mode = "wb")
  nombre_contenido <- unzip(ruta_archivo, list = TRUE)$Name[1]
  nuevo_nombre_archivo <- gsub("\\.[^.]*$", "", nombre_contenido)
  nuevo_nombre_archivo <- paste0(nuevo_nombre_archivo,".zip")
  ruta_nueva <- file.path(ruta_descarga, nuevo_nombre_archivo)
  file.rename(ruta_archivo,ruta_nueva)
  cat(paste0("\n[",k,"] ", normalizePath(ruta_nueva),"\n"))
}
