# Install and load the rvest package
install.packages("rvest")
library(rvest)

# Download the content of a web page
pagina <- read_html("https://www.superbancos.gob.ec/estadisticas/portalestudios/bancos/")

# Extract all download links from the page
download_links <- pagina %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[grepl("download", .)]



library(rvest)

link <- "https://www.superbancos.gob.ec/estadisticas/wp-content/uploads/sites/4/downloads"

# Lee el contenido de la página
pagina <- rvest::read_html(link)

# Selecciona todos los elementos "ancla" que contienen los enlaces de descarga
enlaces <- pagina %>% rvest::html_nodes("a")

# Extrae las URLs de los enlaces de descarga
urls_descarga <- enlaces %>% rvest::html_attr("href")

# Crea las rutas completas de los archivos y carpetas
rutas_completas <- file.path(link, urls_descarga)

# Filtra solo las URLs que terminan en ".zip"
urls_zip <- urls_descarga[grepl("\\.zip$", urls_descarga)]

# Crea las rutas completas de los archivos zip
rutas_completas_zip <- file.path(link, urls_zip)







library(rvest)

# URL de la página que contiene los enlaces de descarga
url <- "https://www.superbancos.gob.ec/estadisticas/portalestudios/bancos/"

# Lee el contenido de la página
pagina <- read_html(url)

# Selecciona el elemento que contiene los enlaces de descarga
contenedor <- pagina %>% html_node(".files-container")

# Selecciona todos los elementos "ancla" que contienen los enlaces de descarga
enlaces <- contenedor %>% html_nodes("a")

# Extrae las URLs de los enlaces de descarga
urls_descarga <- enlaces %>% html_attr("href")
