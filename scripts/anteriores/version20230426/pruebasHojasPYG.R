source("scripts/herramientasImportacionModificacionTablasSB.R")

lista_hojasBAL <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "BALANCE")
lista_hojasPYG <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "PYG")

pyg0 <- compiladorHojasBalanceFinancieroSBP(nombre_hoja_buscada = "PYG")

tabla <- lista_hojasPYG[[102]]
indice_col <- indicePrimeraColDecimalTabla(tabla)
fecha_corte <- identificarFechaCorteBoletinSB(tabla)
  
tabla_limpia <-
  tabla %>%
  eliminarFilasNA(tabla = ., skip = indice_col) %>%
  crearTablaBoletinMensualSB() %>%
  modificarTipoDatoTablaSB() %>%
  mutate(`FECHA` = rep(fecha_corte)) %>%
  select(`FECHA`, everything()) %>%
  modificadorNombresColumnasTablasIF(tabla = .) %>%
  eliminarFilasCODIGOnaCUENTAcod() %>%
  eliminarFilasCODIGOnaCUENTAna()




View(lista_hojasBAL[[1]])

#### Hoja con errores de lectura de fecha [ DESDE EL 01-ENE-2015 AL 31-ENE-2015 ] ####
tabla <- lista_hojasPYG$`BOL BANCOS PRIVADOS 31012015 act.xlsx PYG`
identificarFechaCorteBoletinSB(tabla) # [ "1900-01-03" ]
View(tabla)
indice_col <- indicePrimeraColDecimalTabla(tabla)
indicePrimeraFilDecimalTabla(tabla)
tabla %>%
  eliminarFilasNA(tabla = ., skip = indice_col) %>%
  View()
View(tabla)
  
#### Pruebas ####
tabla <- lista_hojasBAL[[92]]
tabla <- lista_hojasPYG[[102]]
indice_col <- indicePrimeraColDecimalTabla(tabla)
fecha_corte <- identificarFechaCorteBoletinSB(tabla)

tic <- Sys.time()
tabla_limpieza1 <-
  tabla %>%
    eliminarFilasNA(tabla = ., skip = indice_col) %>%
    crearTablaBoletinMensualSB() %>%
    modificarTipoDatoTablaSB() %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(`FECHA`, everything()) %>%
    modificadorNombresColumnasTablasIF(tabla = .) %>%
    eliminarFilasCODIGOnaCUENTAna()
difftime(Sys.time(), tic)

tic <- Sys.time()
tabla_limpieza2 <-
  tabla %>%
    crearTablaBoletinMensualSB() %>%
    eliminarInformacionFinTabla() %>%
    modificarTipoDatoTablaSB() %>%
    eliminarFilasSinValores() %>%
    eliminarColumnasNA() %>%
    mutate(`FECHA` = rep(fecha_corte)) %>%
    select(`FECHA`, everything()) %>%
    modificadorNombresColumnasTablasIF(tabla = .)
difftime(Sys.time(), tic)


#### Tablas con duplicación de columnas mal hecha ####
tabla <- lista_hojasPYG[[102]]
tabla <- lista_hojasPYG[[80]]

indice_col <- indicePrimeraColDecimalTabla(tabla)

tabla_limpia <- tabla %>%
  crearTablaBoletinMensualSB() %>%
  eliminarFilasNA(tabla = ., skip = indice_col) %>%
  View()
View(tabla)




#### AGRUPACION CLUSTERIZACION ####
mi_vector <- unname(evalucionDecimal)
# Realizar análisis de agrupamiento jerárquico
hc <- hclust(dist(mi_vector))
# Dividir el resultado en grupos
grupos <- cutree(hc, k = 2)
grupos
primera_columna_decimal <- which(grupos>1)[1]

