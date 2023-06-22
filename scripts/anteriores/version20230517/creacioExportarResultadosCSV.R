exportarResultadosCSV <- function(tabla, nombre) {
  cat("\n\nExportando resultados...\n")
  ruta_directorio <- "data/Base de Datos"
  crearDirectorio(ruta_directorio)
  nombre_archivo <- paste0(nombre, " ", max(tabla$FECHA), ".csv")
  ruta_archivo <- file.path(ruta_directorio, nombre_archivo)
  data.table::fwrite(tabla, ruta_archivo)
  cat("\nSe ha creado el archivo con la ruta: [", normalizePath(ruta_archivo), "]\n")
}

ruta_directorio <- "data/Base de Datos"
crearDirectorio(ruta_directorio)
nombre_archivo <- paste0("Balance Financiero ", max(BEF$FECHA), ".csv")
ruta_archivo <- file.path(ruta_directorio, nombre_archivo)
data.table::fwrite(BEF, nombre_archivo)

exportarResultadosCSV(BEF,"Balance Financiero")

cat("\n\nExportando resultados...\n")
ruta_directorio <- "data/Base de Datos"
nombre_archivo <- paste0("SEPS Estados Financieros ", max(tabla_concatenada$FECHA), ".csv")
ruta_archivo <- file.path(ruta_directorio,nombre_archivo)
data.table::fwrite(tabla_concatenada, ruta_archivo)
cat("\nSe ha creado el archivo con la ruta: [", normalizePath(ruta_archivo), "]\n")

exportarResultadosCSV(SEPS,"SEPS Estados Financieros")

cat("\n\nExportando resultados...\n")
ruta_directorio <- "data/Base de Datos"
nombre_archivo <- paste0("SB Balances Financieros ", max(consolidada$FECHA), ".csv")
ruta_archivo <- file.path(ruta_directorio,nombre_archivo)
data.table::fwrite(consolidada, ruta_archivo)
cat("\nSe ha creado el archivo con la ruta: [", normalizePath(ruta_archivo), "]\n")

exportarResultadosCSV(SB,"SB Balances Financieros")

