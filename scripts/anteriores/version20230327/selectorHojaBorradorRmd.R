library(tidyverse)
library(readxl)

# Especifica la ruta de la carpeta que contiene los archivos de Excel
ruta_carpeta <- "Anos-anteriores-EEFF-MEN/2016"

# Obtén la lista de archivos de Excel en la carpeta
archivos_excel <- list.files(path = ruta_carpeta, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Crea una vector de la lista de los nombres de las hojas de cada archivo de Excel
lista_nombres_hojas <- unlist(lapply(archivos_excel, function(x) excel_sheets(x)))

# Filtra las hojas que contienen las palabras "estado" y "financiero"
hojas_filtradas <- lista_nombres_hojas[
  str_detect(lista_nombres_hojas, regex("estado", ignore_case = TRUE)) & 
    str_detect(lista_nombres_hojas, regex("financiero", ignore_case = TRUE))]







# Calcular la distancia de Levenshtein entre dos cadenas de texto
distancias_nombres_hojas <- adist("ESTADO FINANCIERO", lista_nombres_hojas)

# Cota empírica para la medida de Levenshtein 2
# mean(head(unique(sort(adist("ESTADO FINANCIERO", lista_nombres_hojas))),3))

# Nombre hojas válidas
nombre_hojas_validas <- unique(lista_nombres_hojas[ which(distancias_nombres_hojas <= 3) ])

archivo <- archivos_excel[1]


# Verificación para todos los archivos de interes en el directorio
for ( archivo in archivos_excel ) {
  # Seleccionamos las hojas que son de nuestro interes 
  hojas_seleccionada <- excel_sheets(archivo)[which( excel_sheets(archivo) %in% hojas_filtradas )]
  # Verificamos si uno de los nombres de hoja está en la lista de interes
  if ( any(excel_sheets(archivo) %in% nombre_hojas_validas) ) {
    print("SI")
  }
}
