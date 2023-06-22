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

#### LECTURA HOJAS EXCEL ####

origen <- "data/Fuente/SB"
archivos <- list.files(origen, recursive = TRUE)
library(readxl)
k <- 180
archivos[k]
mi_df <- read_excel( file.path(origen,archivos[k]), sheet = "BALANCE")
View(mi_df)

# Encontrar la primera fila que contenga la palabra "PICHINCHA"
fila_nombres <- which(apply(mi_df, 1, function(x) any(grepl("PICHINCHA", x, ignore.case = TRUE))))[1]

# Filas en blanco
na_rows <- apply(is.na(mi_df), 1, all)
which(na_rows)

# Transformación Fecha numerica excel
#as.Date(43069, origin = "1899-12-30")
fecha_inicio <- as.numeric(as.Date("2000-01-01")) + 25569
fecha_fin <- as.numeric(as.Date("2100-01-01")) + 25569

# Buscar en la primera fila la celdas que contengan un número de serie de fecha de Excel en las primeras filas antes de los nombres de columna
celdas <- which(mi_df[1:fila_nombres,] >= fecha_inicio & mi_df[1:fila_nombres,] <= fecha_fin, arr.ind = TRUE)
fecha_corte_num <- as.numeric(mi_df[celdas[1], celdas[2]])
fecha_corte_date <-as.Date( fecha_corte_num, origin = "1899-12-30")

# Nueva tabla
new_df <- mi_df[fila_nombres+1:nrow(mi_df), ]
# Columnas con enumeración de filas
row_num_col  <- as.numeric(which(sapply(new_df, function(x) is.numeric(x) && any(diff(na.omit(x)) == 1) )))
# Eliminación de la columna de enumeración de filas
if (length(row_num_col) > 0) {
  new_df[[row_num_col]] <- NULL
}

colnames(new_df) <- as.character(mi_df[fila_nombres,])
new_df$`FECHA CORTE` <- rep(fecha_corte_date, nrow(new_df))
library(dplyr)
new_df <- new_df %>% select(`FECHA CORTE`, everything())

lista_palabras <- c("Fuente", "Elaboración", "Nota", "Grandes", "Medianas", "Pequeñas")
# Filas con coincidencias en la primera columna
matches_col <- grepl(paste(lista_palabras, collapse = "|"), new_df, ignore.case = TRUE)
columna_borrar <- which(matches_col)
matches_row <- grepl(paste(lista_palabras, collapse = "|"), new_df[[1]], ignore.case = TRUE)
filas_borrar <- which(matches_row)
fila_ref_borrar <- min(filas_borrar)
new_df <- mi_df[1:fila_ref_borrar, ]
new_df <- new_df[apply(new_df, 1, function(x) any(!is.na(x))), ]

# Encontrar el número de la columna que contiene algún valor de la secuencia seq(1,3,1)
which(sapply(new_df, function(x) all(seq(100,1000,1) %in% x)))
# Identificar la columna de enumeración de filas
row_num_col <- which(sapply(new_df, function(x) all(diff(x) == 1)))

colum_numericas <- which(sapply(new_df, function(x) is.numeric(x)))

which(sapply(colum_numericas, function(x) diff(new_df[[x]])==1))




# Fundido (Melting) de Tabla
library(reshape2)
#install.packages("reshape2")
df_melted <- melt(new_df, id.vars = colnames(new_df)[1:4], variable.name = "RAZON_SOCIAL", value.name = "SALDO")
View(df_melted)


