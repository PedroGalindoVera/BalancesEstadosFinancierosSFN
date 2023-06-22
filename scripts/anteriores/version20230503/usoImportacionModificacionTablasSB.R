source("scripts/importacionModificacionTablasSB.R")

# Inportación de tabla
origen <- "data/Fuente/SB"
archivos <- list.files(origen, recursive = TRUE)
library(readxl)
k <- 230
archivos[k]
mi_df <- read_excel( file.path(origen,archivos[k]), sheet = "BALANCE")
View(mi_df)

library(dplyr)

new_df <- 
  mi_df %>%
  eliminarColumnasNumeracion() %>%
  eliminarColumnasInsignificantes() %>%
  eliminarFilasSinValores() %>%
  eliminarInformacionFinTabla() %>%
  crearTablaBoletinMensualSB() %>%
  mutate(`FECHA CORTE` = rep(identificarFechaCorteBoletinSB(mi_df))) %>%
  select(`FECHA CORTE`, everything())

# Fundido (Melting) de Tabla
library(reshape2)
#install.packages("reshape2")
df_melted <- melt(new_df, id.vars = colnames(new_df)[1:3], variable.name = "RAZON_SOCIAL", value.name = "SALDO")
View(df_melted)