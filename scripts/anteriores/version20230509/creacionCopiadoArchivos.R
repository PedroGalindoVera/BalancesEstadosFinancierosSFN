# COPIDADO DESDE R ----
ruta_archivo_origen <- normalizePath(ruta_archivo)
ruta_archivo_destino <-
  paste(ruta_directorio_compartido, nombre_archivo, sep = "\\")
file.copy(from = ruta_archivo_origen, to = ruta_archivo_destino)

# COPIADO DESDE cmd ----
cmd <- paste0("robocopy ",
              "\"",normalizePath(dirname(ruta_archivo_origen)), "\" ",
              "\"",normalizePath(dirname(ruta_archivo_destino)), "\" ",
              "\"",basename(ruta_archivo_origen), "\" ", "/MT:16")
system(cmd)