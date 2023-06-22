#descargaEnlacesSelecionados

# Llamado al codigo que analiza los enlaces de descarga
source("analisisEnlacesDescarga.R")

# Imprimir los enlaces
print(url)

# Capetas del directoiro
subcarpetas <- list.dirs(getwd(), recursive = FALSE)
nombres_subcarpetas <- basename(subcarpetas)
nombres_subcarpetas

# Directorio para las descargas
desc_dir <- "Descargas"

# Creamos el directorio para las descargas
if ( !(desc_dir %in% nombres_subcarpetas) ) {
  dir.create(desc_dir)
}

# # Establecer el nombre del archivo de destino
# desc_dir <- file.path(desc_dir, basename(url[[1]]))
# 
# # Descargar de un solo archivo
# download.file(url[[1]], desc_dir)

# Descarga de todos los enlaces
# lapply(url, function(x) download.file(x, file.path(desc_dir, basename(x))))

# # Descarga de todos los enlaces verificando los existentes
# lapply(url, function(x) {
#   dest_file <- file.path(desc_dir, basename(x))
#   if ( !file.exists(dest_file) ) {
#     download.file(x, dest_file)
#   } else {
#     print("El archivo ya existe.")
#   }
# })

# Descarga robusta de todos los enlaces verificando los existentes
for (k in 1:length(url)) {
  dest_file <- file.path(desc_dir, basename(url[[k]]))
  if ( !file.exists(dest_file) ) {
    download.file(url[[k]], dest_file)
  } else {
    print("El archivo ya existe.")
  }
}

# Listamos los archivos comprimidos en el directorio de  Descargas
archivos_comprimidos <- file.path(desc_dir, list.files(desc_dir))

# Consulta meta datos de los archivos comprimidos para buscar subcomprimidos
meta_archivos_comprimidos <- sapply(archivos_comprimidos, function(x) unzip(x, list = TRUE))

# Archivos subcomprimidos
archivos_subcomprimidos <- meta_archivos_comprimidos["Name",]

# Verificamos si hay archivos subcomprimidos y los descomprimimos en Descargas
if (any(grepl("zip", archivos_subcomprimidos))) {
  
  indices_subcomprimidos <- which(grepl("zip", archivos_subcomprimidos))
  
  directorios_comprimidos <- colnames(meta_archivos_comprimidos)[indices_subcomprimidos]
  
  sapply(directorios_comprimidos, function(x) {
    if ( file_ext(x) == "zip" ) {
      unzip(x, exdir = desc_dir)
      file.remove(x)
    }
  })
}

# Establecer el directorio de destino para los archivos Descromprimidos
dest_dir <- "Fuente"

if ( ! (dest_dir %in% nombres_subcarpetas) ) {
  dir.create(dest_dir)
}

# Listo los archivos del directorio
archivos <- list.files(desc_dir)

# Descompresión de archivos
for ( x in archivos ) {
  file <- file.path(desc_dir, basename(x))
  dest_nombre_carpeta <- file.path("Fuente",file_path_sans_ext(x))
  dir.create(dest_nombre_carpeta)
  if ( file_ext(x) == "zip" ) {
    unzip(file, exdir = dest_nombre_carpeta)
  }
}
