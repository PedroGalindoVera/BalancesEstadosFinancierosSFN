source("scripts/importacionModificacionTablasSB.R")

# Inportación de tabla
origen <- "data/Fuente/SB"
archivos <- list.files(origen, recursive = TRUE)
library(readxl)
k <- 251
archivos[k]
tabla <- read_excel( file.path(origen,archivos[k]), sheet = "BALANCE")
View(tabla)

identificarFechaCorteBoletinSB2(tabla)

entradas <- unlist(tabla[1:5,])
grepl("^[0-9]+$", entradas)
parse_date(entradas[!is.na(entradas)])

analisisDifusoNLPFechaCorte(tabla[2,1])

library(dplyr)

new_df <- 
  tabla %>%
  eliminarColumnasNumeracion() %>%
  eliminarColumnasInsignificantes() %>%
  eliminarFilasSinValores() %>%
  eliminarInformacionFinTabla() %>%
  crearTablaBoletinMensualSB() %>%
  mutate(`FECHA CORTE` = rep(identificarFechaCorteBoletinSB(tabla))) %>%
  select(`FECHA CORTE`, everything())

# Fundido (Melting) de Tabla
library(reshape2)
#install.packages("reshape2")
df_melted <- melt(new_df, id.vars = colnames(new_df)[1:3], variable.name = "RAZON_SOCIAL", value.name = "SALDO")
View(df_melted)