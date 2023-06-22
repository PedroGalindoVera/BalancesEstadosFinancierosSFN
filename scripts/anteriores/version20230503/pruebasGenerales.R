source("scripts/herramientasImportacionModificacionTablasSB.R")

lista_hojasBAL <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "BALANCE")
lista_hojasPYG <- listaImportacionDatosFuenteSB(nombre_hoja_buscada = "PYG")

lista_tablasBAL <- listaImportacionModificacionDatosFuenteSB(nombre_hoja_buscada = "BALANCE")

vec <- NULL
lista_prueba <- lista_hojasBAL
s <- sapply(lista_prueba, function(hoja)  {
  tabla <- hojaToTablaBoletinesFinancierosSB(tabla)
  ncol <- ncol(tabla)
  nfil <- nrow(tabla)
  vec <- c(vec, c(ncol,nfil))
  barraProgreso(lista_prueba)
  return(vec)
  })
#sPYG <- as.data.frame(t(s))
sBal <- as.data.frame(t(s))



lista_fechasBAL <-
  sapply(
    lista_hojasBAL,
    function(tabla) {
      fecha <- analisisDifusoNLPFechaCorte(tabla)
      barraProgreso(lista_hojasBAL)
      return(fecha)
    })

fechasBAL  <-
  data.frame( fecha = as.Date(lista_fechasBAL, origin = "1970-01-01") ) %>%
  # Verificamos que no faltan meses between(fecha,27,31)
  arrange(as.Date(fecha)) %>%
  #mutate(diferencia_dias = c(NA, diff(as.Date(fecha)))) %>%
  mutate(diferencia_meses = difftime(fecha, lag(fecha), units = "days"))




lista_fechasPYG <-
  sapply(
    lista_hojasPYG,
    function(tabla) {
      fecha <- analisisDifusoNLPFechaCorte(tabla)
      barraProgreso(lista_hojasPYG)
      return(fecha)
    })

fechasPYG  <-
  data.frame( fecha = as.Date(lista_fechasPYG, origin = "1970-01-01") ) %>%
  # Verificamos que no faltan meses between(fecha,27,31)
  arrange(as.Date(fecha)) %>%
  #mutate(diferencia_dias = c(NA, diff(as.Date(fecha)))) %>%
  mutate(diferencia_meses = difftime(fecha, lag(fecha), units = "days"))
