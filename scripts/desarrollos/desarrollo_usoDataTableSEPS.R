library(data.table)
tic <- Sys.time()
lista_SEPS_DT <- lapply(lista_SEPS, as.data.table)

tabla_concatenada <- rbindlist(lista_SEPS_DT, use.names = TRUE)

tabla_concatenada <- unique(tabla_concatenada)

tabla_concatenada[, `:=`(
  RAZON_SOCIAL = correcionCaracteresVectorizadaSeparada(
    estandarizarCadenaCaracteresSeparada(RAZON_SOCIAL)),
  CUENTA = correcionCaracteresVectorizadaSeparada(
    estandarizarCadenaCaracteresSeparada(CUENTA))
)]
difftime(Sys.time(),tic, units = "auto")

a<-grep("Ó",sample(CUENTA, 100000))

