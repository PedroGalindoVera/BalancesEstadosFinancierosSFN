source("scripts/herramientasImportacionModificacionTablasSB.R")

BalnceFinancieroSB <- compiladorHojasBalanceFinancieroSBP()

BalnceFinancieroSB_PYG <- compiladorHojasBalanceFinancieroSBP(nombre_hoja_buscada = "PYG")

listaPYG <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "PYG")

lista_tablas <- listaPYG
matriz_nombres <-
  t(sapply(lista_tablas, function(x) {
    nombres <- colnames(x)
    length(nombres) <- max(sapply(lista_tablas, function(y) length(colnames(y))))
    return(nombres)
  }))
View(matriz_nombres)


#### BUSCAMOS CELDA PARTICULAR ####

# Texto a buscar
texto_a_buscar <- "* Mediante la resolución No. SB-2014-1052 de 28 de noviembre de 2014 el Banco Promerica se disolvió de forma voluntaria y anticipada."

# Buscar el texto en todas las celdas de todas las tablas
resultado <- lapply(lista_tablas, function(tabla) {
  apply(tabla, MARGIN = 1, function(fila) {
    any(grepl(texto_a_buscar, fila))
  })
})

unlist(resultado)[unname(unlist(resultado))]

"BOL BANCOS PRIVADOS 31122014.xlsx PYG101"

tabla <- listaPYG$`BOL BANCOS PRIVADOS 31122014.xlsx PYG`
View(tabla)

identificarFechaCorteBoletinSB(tabla)
# ARREGLAR LA FECHA


# AGREGAR DONDE CORRESPONDE EXCEPCION PARA BORRAR ESTO
# crear una opcion probabilistica, para reconocer metadata, y borrar informacion
# en virud de cuantas celdas quedan vacias horizontal y verticalmente
# tambien pudiera emplear un condicional, si la celda codigo tiene texto, borrar
