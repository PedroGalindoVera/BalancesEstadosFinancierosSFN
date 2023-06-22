library(tidyverse)
library(readxl)

# Especifica la ruta de la carpeta que contiene los archivos de Excel
ruta_carpeta <- "Anos-anteriores-EEFF-MEN/2016"

# Obtén la lista de archivos de Excel en la carpeta
archivos_excel <- list.files(path = ruta_carpeta, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Crea una lista de los nombres de las hojas de cada archivo de Excel
lista_nombres_hojas <- lapply(archivos_excel, function(x) excel_sheets(x))

# Calcular la distancia de Levenshtein entre dos cadenas de texto
adist("ESTADO FINANCIERO", unlist(lista_nombres_hojas))

#########
# Crea un vector de los nombres de las hojas de cada archivo de Excel
lista_nombres_hojas <- unlist(lapply(archivos_excel, function(x) excel_sheets(x)))
# Calcular la distancia de Levenshtein entre dos cadenas de texto
distancias_nombres_hojas <- adist("ESTADO FINANCIERO", lista_nombres_hojas)
unique(lista_nombres_hojas[which(distancias_nombres_hojas <= 3)])

# Cota empirica para la medida de Levenshtein 2
# mean(head(unique(sort(adist("ESTADO FINANCIERO", unlist(lista_nombres_hojas)))),3))

# Lee cada uno de los archivos de Excel en la lista y crea una lista de data frames
lista_df <- lapply(archivos_excel, function(x) read_excel(x, sheet = "ESTADO FINANCIERO"))

##########
# Leer en el nombre de los archivos y de las carpetas el año

# Combina todos los data frames en uno solo
df_final <- bind_rows(lista_df)

library(readxl)

# Obtener lista de archivos de Excel en la carpeta
archivos_excel <- list.files(path = ruta_carpeta, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Leer solo los archivos con al menos una hoja admisible
hojas_admisibles <- c("Sheet1", "Sheet2")
for (archivo in archivos_excel) {
  hojas_disponibles <- excel_sheets(archivo)
  if (any(hojas_disponibles %in% hojas_admisibles)) {
    datos <- read_excel(archivo, sheet = hojas_admisibles)
    # Hacer algo con los datos leídos
  }
}
