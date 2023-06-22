source("scripts/importacionModificacionTablasSB.R")

#### Importación de Hojas de Cálculo ####
tic <- Sys.time()
tablasSB2  <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "BALANCE")
difftime(Sys.time(), tic, units = "mins")

# Fundido (Melting) de Tabla
library(reshape2)
#install.packages("reshape2")
tabla <- tablasSB2[[1]]
df_melted <- melt(tabla, id.vars = colnames(tabla)[1:3], variable.name = "RAZON_SOCIAL", value.name = "SALDO")
View(df_melted)

df_melted <- lapply(tablasSB2, function(tabla)
  melt(tabla, id.vars = colnames(tabla)[1:3], variable.name = "RAZON_SOCIAL", value.name = "SALDO"))
View(df_melted)




#### Nombres de columnas por tabla ####

lista_tablas <- tablasSB2
matriz_nombres <-
  t(sapply(lista_tablas, function(x) {
    nombres <- colnames(x)
    length(nombres) <- max(sapply(lista_tablas, function(y) length(colnames(y))))
    return(nombres)
}))
View(matriz_nombres)
nombres_BP <- as.vector(matriz_nombres)
nombres_bancos <- sort(unique(nombres_BP))

# Calcular la frecuencia de cada palabra
frecuencias <- table(nombres_BP)
# Crear un histograma
barplot(frecuencias)

  
# Requerimiento de paquetes
if (!require("dplyr")) { 
  install.packages("dplyr")
  library(dplyr)
}

nombres_columnas <-
  sapply(lista_tablas, colnames) %>%
  unlist(.) %>%
  as.character(.) %>%
  unique(.) %>%
  gsub("Á", "A", .) %>%
  gsub("É", "E", .) %>%
  gsub("Í", "I", .) %>%
  gsub("Ó", "O", .) %>%
  gsub("Ú", "U", .) %>%
  gsub("1|BP | S.A.|NA.1|NA.2", "", .) %>%
  gsub("\\bNA\\b", "", .) %>%
  gsub("D MIRO", "D-MIRO", .) %>%
  gsub("BANCOS PRIVADOS MICROCREDITO", "BANCOS PRIVADOS DE MICROCREDITO", .) %>%
  unique(.) %>%
  sort(.)
nombres_columnas <- nombres_columnas[nombres_columnas != ""]



