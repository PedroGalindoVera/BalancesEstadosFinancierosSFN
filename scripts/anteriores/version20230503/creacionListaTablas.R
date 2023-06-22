barraProgreso <- function(conjunto) {
  barra_progreso <- txtProgressBar(min = 0, max = length(conjunto), style = 3)
  nelementos <- length(conjunto)
  if ( exists("contador_progreso") ) {
    setTxtProgressBar(barra_progreso, contador_progreso)
    marcador_cronometro_progreso <- Sys.time()
    tiempo_transcurrido <- difftime(marcador_cronometro_progreso,inicio_cronometro_progreso,units = "sec")
    #estimador_tiempo_restante <- (nelementos - contador_progreso)*(tiempo_transcurrido/contador_progreso)
    estimador_tiempo_proceso <- nelementos*(tiempo_transcurrido/(contador_progreso))
    cat("\nTiempo transcurrido:",
        format(as.POSIXct(as.numeric(tiempo_transcurrido), origin = "1970-01-01", tz = "UTC"),"%H:%M:%S"),
        #"\nTiempo restante:", estimador_tiempo_restante, "minutos")
        " de ", format(as.POSIXct(as.numeric(estimador_tiempo_proceso), origin = "1970-01-01", tz = "UTC"),"%H:%M:%S"),
        "estimados.")
    contador_progreso <<- contador_progreso + 1
  } else {
    inicio_cronometro_progreso <<- Sys.time()
    contador_progreso <<- 1
  }
  cat(paste0("\n[",contador_progreso,"] "))
  if ( contador_progreso == length(conjunto) ) {
    close(barra_progreso)
    rm(contador_progreso, envir = .GlobalEnv)
    rm(inicio_cronometro_progreso, envir = .GlobalEnv)
  }
}

modificadorNombresColumnasTablasIF <- function(catalogo = NULL, tabla) {
  
  # Función para identificar, modificar y eliminar los nombres de las columnas de una tabla utilizando un catálogo de operadores y el método de similitud de cadenas Jaro-Winkler.
  
  # Ejemplo de uso: tabla <- modificadorNombresColumnasTablasIF(tabla = tabla)
  
  if ( is.null(catalogo) ) {
    # Requerimiento de paquetes
    if (!require("readxl")) { 
      install.packages("readxl")
      library(readxl)
    }
    # Leemos el catálogo de una libro de Excel
    catalogo <- readxl::read_excel("data/Otros/Catálogo Operadores.xlsx")
    # Se agrega 0 como número y "0" como carácter para nombres inadecuados en columnas
    catalogo_complemento <- data.frame(
      RUC = rep("0"),
      Operadora = c("FECHA", "CODIGO", "CUENTA",
                    "BANCA MULTIPLE",
                    "BANCOS PRIVADOS GRANDES",
                    "BANCOS PRIVADOS MEDIANOS",
                    "BANCOS PRIVADOS PEQUEÑOS",          
                    "BANCOS PRIVADOS COMERCIALES",
                    "BANCOS PRIVADOS CONSUMO",      
                    "BANCOS PRIVADOS VIVIENDA",
                    "BANCOS PRIVADOS DE MICROEMPRESA",
                    "BANCOS PRIVADOS DE MICROCREDITO",
                    "TOTAL BANCOS PRIVADOS"))
    catalogo <- rbind(catalogo_complemento, catalogo)
  }
  
  # Requerimiento de paquetes
  if (!require("stringdist")) { 
    install.packages("stringdist")
    library(stringdist)
  }
  # Obtener los nombres de las columnas de la tabla
  nombres_columnas <- colnames(tabla)
  # Calcular la similitud de cadenas entre los elementos del catálogo y los nombres de las columnas
  distancia <- as.data.frame(stringdist::stringsimmatrix(catalogo$Operadora, nombres_columnas, method = "jw" ))
  # Colocar los nombres de las filas y columnas en la tabla generada
  colnames(distancia) <- nombres_columnas
  rownames(distancia) <- catalogo$Operadora
  # Identificar el nombre del catálogo con la mayor similitud para cada columna
  identificacion <- sapply(seq_along(distancia), function(columna) row.names(distancia)[which.max(distancia[[columna]])])
  #View(data.frame(original = nombres_columnas, identificacion = identificacion))
  # Renombrar las columnas de la tabla con los nombres identificados
  names(tabla) <- identificacion
  
  return(tabla)
}

# hojaCalculoToTablaSBv0 <- function(ruta_libro, nombre_hoja) { ####
#   
#   # Esta función permite extraer la tabla de datos contenida en un hoja de cálculo correspondiente a los "Boletines Financieros mensuales" de la SB
#   
#   # ARGUMENTOS:
#   # ruta_libro <- "data/Fuente/SB/PRIVADA/2023/FINANCIERO MENSUAL BANCA PRIVADA 2023_02.xlsx"
#   # nombre_hoja <- "BALANCE"
#   # EJEMPLO: tabla <- hojaCalculoToTablaSB(ruta_libro, nombre_hoja)
#   
#   # Requerimiento de paquetes
#   if (!require("readxl")) {
#     install.packages("readxl")
#     library(readxl)
#   }
#   if (!require("dplyr")) {
#     install.packages("dplyr")
#     library(dplyr)
#   }
#   # Importamos una hoja específica de un libro de excel en una ruta determinada
#   hoja <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = FALSE, skip = 0)
#   # Determinamos la fecha más probable contenida en la hoja importada
#   fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
#   # Determinamos la fila más probable con los nombres de las columnas
#   indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
#   # Importamos únicamente la tabla de datos contenida en la hoja especificada, saltando las primeras filas
#   tabla <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, skip = indice_fila_nombres_columnas)
#   # Volvemos a importar la hoja de cálculo pero especificando la fija de inicio, para que se reconozca el tipo de dato y nombre de cada columna
#   tabla <- 
#     tabla %>%
#     # Eliminamos la columna con el nombre "1" solamente si existe
#     select(-one_of("1")) %>%
#     # Eliminamos todas las columnas cuyos nombres empiecen con "..."
#     select(-starts_with("...")) %>%
#     # Eliminamos las filas que contienen únicamente valores NA
#     filter( !if_all(everything(), is.na) ) %>%
#     # Modificamos los nombres de las columnas según un catálogo por defecto
#     modificadorNombresColumnasTablasIF(tabla = .) %>%
#     # Eliminamos todas las filas donde el valor en las columnas "CODIGO" y "CUENTA" es NA
#     filter( !(is.na(CODIGO) & is.na(CUENTA)) ) %>%
#     # Eliminamos las filas donde todas las columnas son NA excepto CUENTA
#     filter( !if_all(-CUENTA, is.na) ) %>%
#     # Agregamos la columna con la fechas de corte
#     mutate(`FECHA` = rep(fecha_corte)) %>%
#     # Movemos la columna FECHA al inicio de la tabla
#     select(`FECHA`, everything())
#   # Agregamos metadatos como atributo de la tabla
#   #attr(tabla, "fecha_creacion") <- Sys.Date()
#   
#   return(tabla)
# } ####

# hojaCalculoToTablaSB <- function(ruta_libro, nombre_hoja) { ####
#   
#   # Esta función permite extraer la tabla de datos contenida en un hoja de cálculo correspondiente a los "Boletines Financieros mensuales" de la SB
#   
#   # ARGUMENTOS:
#   # ruta_libro <- "data/Fuente/SB/PRIVADA/2023/FINANCIERO MENSUAL BANCA PRIVADA 2023_02.xlsx"
#   # nombre_hoja <- "BALANCE"
#   # EJEMPLO: tabla <- hojaCalculoToTablaSB(ruta_libro, nombre_hoja)
#   
#   # Requerimiento de paquetes
#   if (!require("readxl")) {
#     install.packages("readxl")
#     library(readxl)
#   }
#   if (!require("dplyr")) {
#     install.packages("dplyr")
#     library(dplyr)
#   }
#   #
#   ruta_libro <- rutas_libros_seleccionados[85] #83
#   #
#   # Importamos una hoja específica de un libro de excel en una ruta determinada
#   hoja <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = FALSE)
#   # Determinamos la fecha más probable contenida en la hoja importada
#   fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
#   # Determinamos la fila más probable con los nombres de las columnas
#   indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
#   # Inicializamos la variable para almacenar la advertencias
#   advertencias <- NULL
#   # Volvemos a importar la hoja de cálculo pero especificando la fija de inicio, para que se reconozca el tipo de dato y nombre de cada columna
#   tabla <-
#     # Usamos withCallingHandlers() para capturar las advertencias generadas durante la ejecución del código y almacenarlas en una variable
#     withCallingHandlers(
#       # Importamos únicamente la tabla de datos contenida en la hoja especificada, saltando las primeras filas
#       readxl::read_excel(ruta_libro,
#                          sheet = nombre_hoja,
#                          col_names = TRUE,
#                          skip = indice_fila_nombres_columnas,
#                          col_types = c(CODIGO = "text")
#                          ),
#       # Empleamos una función como manejador de advertencias
#       warning = function(w) {
#         # La función toma un argumento w, que es un objeto de advertencia que contiene información sobre la advertencia generada
#         advertencias <<- c(advertencias, w$message)
#         # Suprimimos la advertencia y evitamos que la advertencia se muestre en la consola y permite que el código continúe ejecutándose normalmente
#         invokeRestart("muffleWarning")
#       }
#     )
#   # Agregamos las advertencias como un atributo de la tabla
#   attr(tabla, "advertencias") <- advertencias
#   # Agregamos la columna con la fecha del "Boletín Financiero mensual"
#   tabla <-
#   #tabla_mod <-
#     tabla %>%
#     #
#     mutate(CODIGO = as.numeric(CODIGO)) %>%
#     # # Eliminamos la columna con el nombre "1" solamente si existe
#     # select(-one_of("1")) %>%
#     # # Eliminamos todas las columnas cuyos nombres empiecen con "..."
#     # select(-starts_with("...")) %>%
#     # # Elegimos las columnas con nombres formados por palabras únicamente
#     # select(matches("[[:alpha:]]+$", .)) %>%
#     # Eliminamos las columnas que no contengan caracteres alfabéticos
#     select( -matches("^[^[:alpha:]]+$", .) ) %>%
#     # Eliminamos las filas que contienen únicamente valores NA
#     filter( !if_all(everything(), is.na) ) %>%
#     # Empleamos la función creada para modificar los nombres de las columnas según un catálogo por defecto
#     modificadorNombresColumnasTablasIF(tabla = .) %>%
#     # Eliminamos todas las filas donde el valor en las columnas "CODIGO" y "CUENTA" es NA
#     filter( !(is.na(CODIGO) & is.na(CUENTA)) ) %>%
#     # Eliminamos las filas donde todas las columnas son NA excepto CUENTA
#     filter( !if_all(-CUENTA, is.na) ) %>%
#     # Eliminamos las filas donde la columna CODIGO tenga letras mientras todas las las demás columnas son NA
#     filter( !(grepl("[[:alpha:]]+",CODIGO) & if_all(-CODIGO, is.na)) ) %>%
#     # Agregamos la columna con la fechas de corte
#     mutate(`FECHA` = rep(fecha_corte)) %>%
#     # Movemos la columna FECHA al inicio de la tabla
#     select(`FECHA`, everything())
#   # Agregamos metadatos como atributo de la tabla
#   #attr(tabla, "fecha_creacion") <- Sys.Date()
#   
#   return(tabla)
# } ####

hojaToTablaBoletinesFinancierosSB <- function(ruta_libro, nombre_hoja, fecha_corte = NULL) {
  
  # Esta función permite extraer la tabla de datos contenida en un hoja de cálculo correspondiente a los "Boletines Financieros mensuales" de la SB
  
  # ARGUMENTOS:
  # ruta_libro <- "data/Fuente/SB/PRIVADA/2023/FINANCIERO MENSUAL BANCA PRIVADA 2023_02.xlsx"
  # nombre_hoja <- "BALANCE"
  # fecha_corte <- "2023-02-29"
  # EJEMPLO: tabla <- hojaToTablaBoletinesFinancierosSB(ruta_libro, nombre_hoja, fecha_corte)
  
  # Requerimiento de paquetes
  if (!require("readxl")) {
    install.packages("readxl")
    library(readxl)
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  #
  #ruta_libro <- rutas_libros_seleccionados[85] #83
  #
  # Importamos las 30 primeras filas de una hoja específica de un libro de excel en una ruta determinada
  hoja <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = FALSE, n_max = 30)
  # # Determinamos la fecha más probable contenida en la hoja importada
  # fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
  # Regla de decisión para la fecha de corte
  fecha_corte <-
    if ( is.null(fecha_corte) ) {
      # Determinamos la fecha más probable contenida en la hoja importada
      analisisDifusoNLPFechaCorte(hoja)
    } else {
      fecha_corte
    }
  # Determinamos la fila más probable con los nombres de las columnas
  indice_fila_nombres_columnas <- indicePrimeraFilDecimalTabla(hoja) - 1
  # Almacenamos la fila con los nombres de las columnas
  nombres_columnas <- unname(unlist(hoja[indice_fila_nombres_columnas,]))
  # Importamos una tabla de prueba para verificar la correcta asignación de los nombres de las columnas en sus 20 primeras filas
  tabla_prueba <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20)
  # Verificamos si coinciden adecuadamente los nombres de las columnas
  if ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 ) {
    # Retrocedemos un índice en las filas previo a iterear para incluir cualquier caso exepcional
    indice_fila_nombres_columnas <- indice_fila_nombres_columnas - 2
    # Iteramos hasta que hayan coincidencias en al menos el 80%
    while ( mean(nombres_columnas == names(tabla_prueba), na.rm = TRUE) < 0.8 & indice_fila_nombres_columnas <= 20 ) {
      # Incrementamos el índice de la fila para continuar la prueba
      indice_fila_nombres_columnas <- indice_fila_nombres_columnas + 1
      # Reimportamos la tabla de prueba para verificar la correcta asignación de los nombres de las columnas en sus 20 primeras filas
      tabla_prueba <- readxl::read_excel(ruta_libro, sheet = nombre_hoja, col_names = TRUE, skip = indice_fila_nombres_columnas, n_max = 20)
    }
  }
  # Inicializamos la variable para almacenar la advertencias
  advertencias <- NULL
  # Volvemos a importar la hoja de cálculo pero especificando la fija de inicio, para que se reconozca el tipo de dato y nombre de cada columna
  tabla <-
    # Usamos withCallingHandlers() para capturar las advertencias generadas durante la ejecución del código y almacenarlas en una variable
    withCallingHandlers(
      # Importamos únicamente la tabla de datos contenida en la hoja especificada, saltando las primeras filas
      readxl::read_excel(ruta_libro,
                         sheet = nombre_hoja,
                         col_names = TRUE,
                         skip = indice_fila_nombres_columnas),
      # Empleamos una función como manejador de advertencias
      warning = function(w) {
        # La función toma un argumento w, que es un objeto de advertencia que contiene información sobre la advertencia generada
        advertencias <<- c(advertencias, w$message)
        # Suprimimos la advertencia y evitamos que la advertencia se muestre en la consola y permite que el código continúe ejecutándose normalmente
        invokeRestart("muffleWarning")
      }
    )
  # Agregamos las advertencias como un atributo de la tabla
  attr(tabla, "advertencias") <- advertencias
  # Agregamos la columna con la fecha del "Boletín Financiero mensual"
  tabla <-
    tabla %>%
    # # Eliminamos la columna con el nombre "1" solamente si existe
    # select(-one_of("1")) %>%
    # # Eliminamos todas las columnas cuyos nombres empiecen con "..."
    # select(-starts_with("...")) %>%
    # # Elegimos las columnas con nombres formados por palabras únicamente
    # select(matches("[[:alpha:]]+$", .)) %>%
    # Eliminamos las columnas que no contengan caracteres alfabéticos
    select( -matches("^[^[:alpha:]]+$", .) ) %>%
    # Eliminamos las filas que contienen únicamente valores NA
    filter( !if_all(everything(), is.na) ) %>%
    # Empleamos la función creada para modificar los nombres de las columnas según un catálogo por defecto
    modificadorNombresColumnasTablasIF(tabla = .) %>%
    # Modificamos la columna CODIGO a texto
    mutate(CODIGO = as.character(CODIGO)) %>%
    # Modificamos la columna CUENTA a texto
    mutate(CUENTA = as.character(CUENTA)) %>%
    # Modificamos el resto de columnas a numéricas
    mutate_at(vars(-CODIGO, -CUENTA), as.numeric) %>%
    # Eliminamos todas las filas donde el valor en las columnas "CODIGO" y "CUENTA" es NA
    filter( !(is.na(CODIGO) & is.na(CUENTA)) ) %>%
    # Eliminamos las filas donde todas las columnas son NA excepto CUENTA
    filter( !if_all(-CUENTA, is.na) ) %>%
    # Eliminamos las filas donde la columna CODIGO tenga letras mientras todas las las demás columnas son NA
    filter( !(grepl("[[:alpha:]]+",CODIGO) & if_all(-CODIGO, is.na)) ) %>%
    # Agregamos la columna con la fechas de corte
    mutate(`FECHA` = rep(fecha_corte)) %>%
    # Movemos la columna FECHA al inicio de la tabla
    select(`FECHA`, everything())
  # Agregamos metadatos como atributo de la tabla
  #attr(tabla, "fecha_creacion") <- Sys.Date()
  
  return(tabla)
}


ruta_directorio <- "data/Fuente/SB/PRIVADA"
nombre_hoja <- "BALANCE"
archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
rutas_libros <- file.path(ruta_directorio, archivos_directorio)
prueba_anio <- grepl("(201[3-9])|(202[0-9])",rutas_libros)
rutas_libros_seleccionados <- rutas_libros[prueba_anio]


hoja <- readxl::read_excel(rutas_libros_seleccionados[1], sheet = nombre_hoja)

tabla <- hojaToTablaBoletinesFinancierosSB (rutas_libros_seleccionados[1], nombre_hoja)



lista_tablas <-
  lapply(#head(rutas_libros_seleccionados,122),
         rutas_libros_seleccionados[-c(61,85)],
         function(ruta_libro) {
           barraProgreso(rutas_libros_seleccionados)
           tabla <- hojaCalculoToTablaSB(ruta_libro, nombre_hoja)
           return(tabla)
          })


rutas_libros_seleccionados <- rutas_libros[prueba_anio]
#rutas_libros_seleccionados <- head(rutas_libros_seleccionados,122)
#rutas_libros_seleccionados <- rutas_libros_seleccionados[-c(61)]
rm(contador_progreso)
lista_tablas <- list()
for ( ruta_libro in rutas_libros_seleccionados ) {
  nombre_tabla <- basename(ruta_libro)
  lista_tablas[[nombre_tabla]] <- hojaCalculoToTablaSB(ruta_libro, nombre_hoja)
  barraProgreso(rutas_libros_seleccionados)
}

