barraProgresoReinicio()
lista_tablas_BAL <- list()
lista_tablas_PYG <- list()
for ( ruta_libro in rutas_libros_seleccionados[1:4] ) {
  hoja <-
    suppressMessages(
      readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20))
  fecha_corte <- analisisDifusoNLPFechaCorte(hoja)
  tabla_BAL <-
    hojaToTablaBoletinesFinancierosSB_v2(ruta_libro, "BALANCE", fecha_corte)
  tabla_PYG <-
    hojaToTablaBoletinesFinancierosSB_v2(ruta_libro, "PYG", fecha_corte)
  nombre_tabla <- basename(ruta_libro)
  lista_tablas_BAL[[nombre_tabla]] <- tabla_BAL
  lista_tablas_PYG[[nombre_tabla]] <- tabla_PYG
  barraProgreso(rutas_libros_seleccionados)
  cat("\033[1;32mImportando y procesando el archivo:\033[0m",
      "[", normalizePath(ruta_libro), "]\n")
}

lista_tablas <- c(lista_tablas_BAL,lista_tablas_PYG)

k <- 1

lista_tablas[[k]]%>%View()

lista_tablas[[k]] %>% colnames()

lista_tablas[[k]] %>%
  modificarNombreColumnaSB_v2() %>%
  select( -matches("^[^[:alpha:]]+$", .) ) %>%
  filter_all( any_vars( ! is.na(.) ) ) %>%
  mutate(
    CODIGO = as.character(CODIGO),
    CUENTA = as.character(CUENTA)) %>%
  mutate_at( vars(-CODIGO, -CUENTA), as.numeric) %>%
  filter( ! is.na(CODIGO) & ! is.na(CUENTA) ) %>%
  mutate( FECHA = fecha_corte ) %>%
  select( FECHA, everything() ) %>%
  View()

depurarTablaSB <- function(tabla) {
  tabla %>%
    modificarNombreColumnaSB_v2() %>%
    select( -matches("^[^[:alpha:]]+$", .) ) %>%
    filter_all( any_vars( ! is.na(.) ) ) %>%
    mutate(
      CODIGO = as.character(CODIGO),
      CUENTA = as.character(CUENTA)) %>%
    mutate_at( vars(-CODIGO, -CUENTA), as.numeric) %>%
    filter( ! is.na(CODIGO) & ! is.na(CUENTA) ) %>%
    mutate( FECHA = fecha_corte ) %>%
    select( FECHA, everything() )
}

lista_tablas[[k]] %>% depurarTablaSB() %>% View()


ruta_libro <- "data/Fuente/SB/Boletines Financieros Mensuales/Bancos Privados/2015/2015/BOL BANCOS PRIVADOS 28022015 act.xlsx"

A <- hojaToTablaBoletinesFinancierosSB(ruta_libro, "BALANCE", fecha_corte = NULL)


