source("scripts/importacionModificacionTablasSB.R")

# Inportación de tabla

if (!require("readxl")) { install.packages("readxl") }
library(readxl)

folder_path <- "data/Fuente/SB"
files <- list.files(folder_path, recursive = TRUE)
#files <- list.files(folder_path, pattern = "\\.xlsx")

data <- list()

for (file in files) {
  file_path <- file.path(folder_path, file)
  sheet_names <- excel_sheets(file_path)
  data[[file]] <- sheet_names
}

max_length <- max(sapply(data, length))
data_padded <- lapply(data, function(x) {c(x, rep(NA, max_length - length(x)))})
df <- as.data.frame(data_padded, stringsAsFactors = FALSE)
df <- as.data.frame(t(df))
colnames(df) <- paste("Hoja",seq(1,max_length))
View(df)

# Obtener valores unicos de las celdas
unique_values <- unique(as.vector(as.matrix(df)))
# ordenamos alfabeticamente
sorted_values <- sort(unique_values)
# coincidencias, Then it applies the agrep function to find all the values that are similar to the word “balance”. The agrep function returns the indices of the matching values
matches <- unique_values[agrep("bal", sorted_values)]


#### distancia entre palabras ####

##### Paquete stringdist #####
if (!require("stringdist")) { install.packages("stringdist") }
library(stringdist)

# Distancia de Levenshtein 
matriz_distancia <- as.matrix(stringdistmatrix(sorted_values, method = "lv"))
colnames(matriz_distancia) <- sorted_values
rownames(matriz_distancia) <- sorted_values
View(matriz_distancia)
indices <- which(matriz_distancia < 5, arr.ind = TRUE)
out <- matriz_distancia[indices]
View(out)



library(stringdist)

palabras <- c("gato", "perro", "pájaro", "pez", "conejo", "ratón")
palabra_seleccionada <- "gatito"

distancias <- stringdist(palabras, palabra_seleccionada, method = "lv")
umbral <- 2
palabras_seleccionadas <- palabras[distancias <= umbral]


distancias <- stringdist(palabras, palabra_seleccionada, method = "lv")
umbral <- 2
palabras_seleccionadas <- palabras[distancias <= umbral]


nombres_hojas_libros <- tolower(sort(unique(as.vector(as.matrix(df)))))
nombre_hoja_buscada <- "balance"
distancias <- stringdist(nombres_hojas_libros, nombre_hoja_buscada, method = "lv")
min(distancias, na.rm = TRUE)
max(distancias, na.rm = TRUE)
mean(distancias, na.rm = TRUE)
umbral <- 5
nombres_hojas_seleccionadas <- nombres_hojas_libros[distancias <= umbral]



distancias <- stringdist(nombres_hojas_libros, nombre_hoja_buscada, method = "qgram")
hist(distancias)
umbral <- 8
nombres_hojas_seleccionadas <- nombres_hojas_libros[distancias <= umbral]
