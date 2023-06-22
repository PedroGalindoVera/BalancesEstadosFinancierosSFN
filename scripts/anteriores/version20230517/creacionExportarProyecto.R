exportarProyecto <- function() {
  crearDirectorioNormalizado <- function(ruta_directorio) {
    ruta_directorio_normalizada <- normalizePath(ruta_directorio)
    if (!dir.exists(ruta_directorio_normalizada)) {
      dir.create(ruta_directorio_normalizada, recursive = TRUE)
      cat("\nSe creo la carpeta: [", basename(ruta_directorio_normalizada), "]",
          "con la ruta: [", ruta_directorio_normalizada, "].\n")
    }
  }
  ruta_directorio_compartido <- "\\\\192.168.10.244\\inteligencia"
  directorio_proyecto <- "DESARROLLO\\BalancesEstadosFinacierosSFN"
  ruta_directorio_proyecto <-
    paste(ruta_directorio_compartido, directorio_proyecto, sep = "\\")
  crearDirectorioNormalizado(ruta_directorio_proyecto)
  ruta_directorio_scripts <-
    paste(ruta_directorio_proyecto, "scripts", sep = "\\")
  crearDirectorioNormalizado(ruta_directorio_scripts)
  ruta_directorio_data <-
    paste(ruta_directorio_proyecto, "data", sep = "\\")
  crearDirectorioNormalizado(ruta_directorio_data)
  archivos_a_copiar_en_scripts <- c(
    "scripts/herramientasBalancesEstadosFinancieros.R",
    "scripts/usoCreacionBalancesEstadosFinancieros.R")
  file.copy(
    from = archivos_a_copiar_en_scripts, to = ruta_directorio_scripts)
  file.copy(from = "data/Catalogos", to = ruta_directorio_data, recursive = TRUE)
  file.copy(from = "html", to = ruta_directorio_proyecto, recursive = TRUE)
  file.copy(from = "installers", to = ruta_directorio_proyecto, recursive = TRUE)
  file.copy(
    from = "BalancesEstadosFinacierosSFN.Rproj", to = ruta_directorio_proyecto)
}

exportarProyecto()










