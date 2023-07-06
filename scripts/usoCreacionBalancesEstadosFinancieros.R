tic_general <- Sys.time()

source("scripts/herramientasBalancesEstadosFinancieros.R")

configuracionLocal()

requerirPaquetes()

SEPS <- ejecutarProcesoSEPS()

Sys.sleep(10)

generarCatalogosSEPS(SEPS,"SEPS")

SB <- ejecutarProcesoSB()

Sys.sleep(10)

generarCatalogosSEPS(SB,"SB")

BEF <- rbind(SEPS, SB)

Sys.sleep(10)

generarCatalogosSEPS(BEF,"SFN")

exportarResultadosCSV(BEF,"Balances Estados Financieros")

ruta_dir_compartida <- "\\\\192.168.10.244\\inteligencia"
exportarResultadosCSV(BEF,"Balances Estados Financieros",ruta_dir_compartida)

configuracionLocal(restaurar = TRUE)

cat("\n\n  \033[1;34mDuración total del proceso:",
    formatoTiempoHMS(difftime(Sys.time(), tic_general, units = "secs")), "\033[0m\n\n")