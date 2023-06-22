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
