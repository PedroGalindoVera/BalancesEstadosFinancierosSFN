source("scripts/herramientasImportacionModificacionTablasSB.R")

compilarHojasBalanceFinancieroSB <- function(ruta_directorio = NULL) {
  
  # Esta función realiza todo el proceso necesario para crear la base de datos de los Balances Financieros mensuales de la SB
  
  # Requerimiento de paquetes
  if (!require("readxl")) { 
    install.packages("readxl")
    library(readxl)
  }
  
  # Requerimiento de paquetes
  if (!require("dplyr")) { 
    install.packages("dplyr")
    library(dplyr)
  }
  
  # Requerimiento de paquetes
  if (!require("reshape2")) { 
    install.packages("reshape2")
    library(reshape2)
  }
  
  # Establecemos la ruta del directorio fuente de los libros de Excel con los "Boletines Financieros mensuales"
  if ( is.null(ruta_directorio) ) ruta_directorio <- "data/Fuente/SB/PRIVADA"
  # Determinamos los archivos presentes en directorio fuente
  archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
  # Determinamos todas las rutas de los archivos en el directorio
  rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  # Determinamos los archivos a transformar de formato
  rutas_transformar <- rutas_libros[tools::file_ext(rutas_libros) == "xlsb"]
  # Realizamos los cambios solo si son necesarios
  if ( length(rutas_transformar) > 0 ) {
    # Cambiar el formato
    purrr::map(rutas_transformar, xlsb2xlsx)
    # Volvemos a determinar los archivos presentes en directorio fuente
    archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
    # Volvemos a determinar todas las rutas de los archivos en el directorio luego del cambio de formato
    rutas_libros <- file.path(ruta_directorio, archivos_directorio)
  }
  # Establecemos una prueba con expresión regular para filtrar los años 2013-2029
  prueba_anio <- grepl("(201[3-9])|(202[0-9])",rutas_libros)
  # Filtramos las rutas con los años establecidos
  rutas_libros_seleccionados <- rutas_libros[prueba_anio]
  # Limpiamos la barra de progreso
  rm(contador_progreso, envir = .GlobalEnv)
  # Inicializamos la lista de las tablas concatenadas de BALANCE y PYG
  lista_tablas_BAL_PYG_concatenadas <- list()
  # Definimos el bucle de ejecución
  for ( ruta_libro in rutas_libros_seleccionados ) {
    # Importamos las 20 primeras filas de la hoja BALANCE para identificar la fecha de corte
    hoja <- suppressMessages(readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
    # Identificamos la fecha de corte
    fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
    # Extraemos la tabla de BALANCE
    tabla_BAL <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte)
    # Extraemos la tabla de PYG
    tabla_PYG <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "PYG", fecha_corte)
    # Definimos el nombre de para cada tabla
    nombre_tabla <- basename(ruta_libro)
    # Asignamos la tabla concatenada de BALANCE y PYG a un elemento de la lista de tablas
    lista_tablas_BAL_PYG_concatenadas[[nombre_tabla]] <- dplyr::bind_rows(tabla_BAL,tabla_PYG)
    # Ejecutamos el código para la barra de progreso
    barraProgreso(rutas_libros_seleccionados)
    # Mostramos la ruta del archivo en proceso
    cat(normalizePath(ruta_libro),"\r")
  }
  # Concatenamos todas las tablas de la lista generada
  tabla_BAL_PYG <- dplyr::bind_rows(lista_tablas_BAL_PYG_concatenadas)
  # Asignamos el registro completo de advertencias (warnings) generadas al convertir a tabla las hojas de cálculo
  registro_advertencias <-
    sapply(seq_along(lista_tablas_BAL_PYG_concatenadas),
           function(k) attr(lista_tablas_BAL_PYG_concatenadas[[k]],"advertencias"))
  # Recuperamos los nombres de cada archivo para el registro de advertencias
  names(registro_advertencias) <- names(lista_tablas_BAL_PYG_concatenadas)
  # Asignamos la información de las advertencias a un data frame
  reporte_consolidacion_BAL_PYG <-
    data.frame(
      Archivo = names(unlist(registro_advertencias)),
      Advertencia = unname(unlist(registro_advertencias)))
  # Exportamos el reporte con el registro de las advertencias
  exportarReporteTabla(reporte_consolidacion_BAL_PYG,
                       paste("Reporte Advertencias en Consolidación Balances Financieros SB",
                             basename(ruta_directorio)))
  # Fundido (melting) de tabla
  tabla_BAL_PYG_fundida <-
    reshape2::melt(tabla_BAL_PYG,
                   id.vars = colnames(tabla_BAL_PYG)[1:3],
                   variable.name = "RAZON_SOCIAL",
                   value.name = "VALOR")
  
  return(tabla_BAL_PYG_fundida)
}

PUBLICA <- compilarHojasBalanceFinancieroSB(ruta_directorio = "data/Fuente/SB/PUBLICA")

PRIVADA <- compilarHojasBalanceFinancieroSB(ruta_directorio = "data/Fuente/SB/PRIVADA")

