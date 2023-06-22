#### Descarga y descompresión de datos ####

source("scripts/decargaTablasBalanceFinancieroSEPS.R")

descompresionArchivosSubcomprimidos()

descompresionArchivos()

rm(
  cracionDirectorio,
  exportarReporteTabla,
  analisisVinculosPaginaWeb,
  descargaArchivosEnlacesAnalizados,
  descompresionArchivos,
  descompresionArchivosSubcomprimidos,
  obtencionEnlacesDescarga
)

#### Importación y manipulación de tablas de datos ####

source("scripts/importacionModificacionTablasSEPS.R")

# Creamos una lista con todas las tablas
lista_tablas <- listaImportacionDatosFuente()
# Vector para los índices de la lista
indices_lista_datos <- 1:length(lista_tablas)
# Homologamos los nombres de todas las columnas de las tablas en la lista
lista_tablas <- lapply(indices_lista_datos, function(x) setModificadorNombresColumnas(lista_tablas[[x]]))

rm(
  indices_lista_datos,
  listaImportacionDatosFuente,
  modificadorNombreColumna,
  setModificadorNombresColumnas
)

#### Consolidación y unificación de tabla de datos ####

if (!require("dplyr")) { install.packages("dplyr") }
library(dplyr)
if (!require("lubridate")) { install.packages("lubridate") }
library(lubridate)

# Cambiamos el tipo de dato de la columna "Fecha de Corte" a date en toda la lista
lista_tablas <- lapply(lista_tablas, function(x) {
  x %>%
    dplyr::mutate(
      `Fecha de Corte` = lubridate::ymd(`Fecha de Corte`)
    )
})

lista_tablas <- lapply(lista_tablas, function(x) {
  x %>%
    dplyr::mutate(
      `Saldo (USD)` = as.numeric(gsub(",", ".", `Saldo (USD)`))
    )
})

# Juntamos las tablas de toda la lista en una sola
tabla_combinada <- dplyr::bind_rows(lista_tablas)

rm(lista_tablas)

# Modificación en segmento para Mutualistas
tabla_combinada$Segmento[tabla_combinada$Segmento == "SEGMENTO 1 MUTUALISTA"] <- "MUTUALISTA"

#### Exportacion de tabla ####
# start timer
ptm <- proc.time()
# Exportamos la tabla consolidada
write.csv(tabla_combinada, "data/Base de Datos/SEPS Balance Financiero.csv", row.names = FALSE)
# stop timer and calculate elapsed time
elapsed_time <- proc.time() - ptm


unique(tabla_combinada$Segmento)
length( unique(tabla_combinada$`Razón Social`) )
length( unique(tabla_combinada$RUC) )
length( unique(tabla_combinada$Cuenta) )
length( unique(tabla_combinada$Descripción) )
min(tabla_combinada$`Fecha de Corte`)
max(tabla_combinada$`Fecha de Corte`)
fechaCorte <- tabla_combinada %>%
  distinct(`Fecha de Corte`) %>%
  arrange(`Fecha de Corte`) %>%
  mutate(Periodo = toupper(format(`Fecha de Corte`, "%Y %b")))

#### Determinación de los RUC con varias Razones Sociales ####
# RUC registrados
RUC <- unique(tabla_combinada$RUC) 
# Razones Sociales registradas
RazonSocial <- sapply(RUC, function(x) unique(tabla_combinada$`Razón Social`[tabla_combinada$RUC == x]))
# RUC con multiasignados con Razones Sociales
RUC_multiasignados <- names(which(sapply(RazonSocial, length) > 1))
# Registros multiasignados de Razon Social con su RUC
Registros_Multiasignados <- sapply(RUC_multiasignados, function(x) RazonSocial[[x]])

# Reporte de multiasignaciones
max_length <- max(sapply(Registros_Multiasignados, length))
df <- data.frame(lapply(Registros_Multiasignados, function(x) {
  length(x) <- max_length
  return(x)
}))
df <- rbind(names(Registros_Multiasignados), df)
df <- as.data.frame(t(df))
#rownames(df) <- NULL
View(df)
# Establecer el directorio de destino para los Reportes
# subcarpetas <- list.dirs("data", recursive = FALSE)
# nombres_subcarpetas <- basename(subcarpetas)
# rep_dir <- "data/Reportes"
# cracionDirectorio <- function(directorio_base = "data", rep_dir)
# library(openxlsx)
# write.xlsx(df, file.path(rep_dir,"Nombres Razon Social.xlsx"), rowNames = FALSE)
exportarReporteTabla(dataFrame = df, nombre_archivo = "Nombres Razon Social")


#### Determinación de caracteres no hispanos ####
# Vector con carateres admisibles
# caracteres_admisibles <- "[^a-zA-ZáéíóúüñÁÉÍÓÚÜÑ[:punct:][:space:]]"
caracteres_admisibles <- "[^a-zA-ZáéíóúüñÁÉÍÓÚÜÑ[:space:]]"
# El resultado sería un valor lógico que indica si se encontró algún carácter que no esté en el conjunto de caracteres permitidos.
grepl(caracteres_admisibles, Registros_Multiasignados[[1]][1])



