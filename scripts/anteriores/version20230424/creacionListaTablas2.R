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
archivos_directorio <- list.files(ruta_directorio, recursive = TRUE)
rutas_libros <- file.path(ruta_directorio, archivos_directorio)
prueba_anio <- grepl("(201[3-9])|(202[0-9])",rutas_libros)
rutas_libros_seleccionados <- rutas_libros[prueba_anio]
rm(contador_progreso)
ruta_libro <- rutas_libros_seleccionados[1]
lista_tablas_BAL_PYG_concatenadas <- list()
for ( ruta_libro in rutas_libros_seleccionados ) {
  # Importamos las 20 primeras filas de la hoja BALANCE para identificar la fecha de corte
  hoja <- readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20)
  fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
  tabla_BAL <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte)
  tabla_PYG <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "PYG", fecha_corte)
  nombre_tabla <- basename(ruta_libro)
  lista_tablas_BAL_PYG_concatenadas[[nombre_tabla]] <- dplyr::bind_rows(tabla_BAL,tabla_PYG)
  barraProgreso(rutas_libros_seleccionados)
}

tabla_BAL_PYG <- dplyr::bind_rows(lista_tablas_BAL_PYG_concatenadas)


s <- sapply(seq_along(lista_tablas_BAL_PYG_concatenadas),
            function(k) attr(lista_tablas_BAL_PYG_concatenadas[[k]],"advertencias"))
names(s) <- names(lista_tablas_BAL_PYG_concatenadas)

list2DF(s) %>% View

