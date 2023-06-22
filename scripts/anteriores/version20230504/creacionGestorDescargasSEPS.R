source("scripts/herramientasConsolidacionTablaSB-DESKTOP-CTJM007.R")

gestorDescargasSEPS <- function() {
  
  enlaces_SEPS <- analisisVinculosPaginaWebSEPS()
  
  info_enlaces_SEPS <- obtencionEnlacesDescarga(enlaces_descarga = enlaces_SEPS, identificador = "SEPS")
  
  ruta_descargas_SEPS <- "data/Descargas/SEPS/Bases de Datos"
  descargaArchivosEnlacesAnalizados(enlaces_SEPS, info_enlaces_SEPS, ruta_descargas_SEPS)
  
  ruta_fuentes_SEPS <- "data/Fuente/SEPS/Bases de Datos"
  descompresionArchivosDirectorio(ruta_descargas_SEPS, ruta_fuentes_SEPS)
  
}

gestorDescargasSEPS()
