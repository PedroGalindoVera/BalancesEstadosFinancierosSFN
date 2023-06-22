##### SEPS #####

enlacesSEPS <- analisisVinculosPaginaWebSEPS()

informacionSEPS <- obtencionEnlacesDescarga(enlaces_descarga = enlacesSEPS, identificador = "SEPS")

descargaArchivosEnlacesAnalizados(enlacesSEPS, informacionSEPS, "data/Descargas/SEPS/Bases de Datos")

descompresionArchivosSubcomprimidos(ruta = "data/Descargas/SEPS")

descompresionArchivos(origen = "data/Descargas/SEPS", destino = "data/Fuente/SEPS")