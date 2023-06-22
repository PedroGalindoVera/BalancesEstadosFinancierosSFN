lista_paquetes_empleados <-
  list(
    "beepr",
    "data.table", "dplyr",
    "httr",
    "lubridate",
    "openxlsx",
    "parallel", "parsedate", "purrr",
    "readr", "readxl", "reshape2", "rlang", "rvest",
    "stats", "stringdist", "stringr",  
    "tools",
    "utils"
  )
names(lista_paquetes_empleados) <- unlist(lista_paquetes_empleados)

paquetes_y_versiones_empleados <-
  lapply(lista_paquetes_empleados, function(paquete)
    as.character(packageVersion(paquete)))
names(paquetes_y_versiones_empleados) <- unlist(lista_paquetes_empleados)

for (paquete in names(paquetes_y_versiones_empleados)) {
  version_requerida <- paquetes_y_versiones_empleados[[paquete]]
  version_actual <- packageVersion(paquete)
  if (package_version(version_actual) < package_version(version_deseada)) {
    install.packages(
      paquete,
      repos = paste0("http://cran.r-project.org/bin/windows/contrib/",
                     getRversion(), "/", paquete, "_", version_deseada, ".zip")
      # repos = paste0("http://cran.r-project.org/src/contrib/Archive/",
      #                paquete, "/", paquete, "_", version_requerida, ".tar.gz")
    )
  }
}





paquetes_desactualizados <- utils::old.packages()

paquetes_a_actualizar <-
  intersect(
    rownames(paquetes_desactualizados),
    unlist(lista_paquetes_empleados))

if (length(paquetes_a_actualizar) > 0) {
  update.packages(pkgs = paquetes_a_actualizar, ask = FALSE)
}



# Crear lista de paquetes y versiones deseadas
paquetes_y_versiones <- list(
  "beepr" = "1.3",
  "data.table" = "1.14.8",
  "dplyr" = "1.1.2",
  "httr" = "1.4.6",
  "lubridate" = "1.9.2",
  "openxlsx" = "4.2.5.2",
  "parallel" = "4.3.0",
  "parsedate" = "1.3.1",
  "purrr" = "1.0.1",
  "readr" = "2.1.4",
  "readxl" = "1.4.2",
  "reshape2" = "1.4.4",
  "rlang" = "1.1.1",
  "rvest" = "1.0.3",
  "stats" = "4.3.0",
  "stringdist" = "0.9.10",
  "stringr" = "1.5.0",
  "tools" = "4.3.0",
  "utils" = "4.3.0"
)

# Instalar versiones específicas de cada paquete
for (paquete in names(paquetes_y_versiones)) {
  version_deseada <- paquetes_y_versiones[[paquete]]
  
  install.packages(
    paquete,
    repos = paste0("http://cran.r-project.org/src/contrib/Archive/", paquete, "/", paquete, "_", version_deseada, ".tar.gz")
  )
}
