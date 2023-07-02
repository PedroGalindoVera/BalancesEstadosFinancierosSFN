reemplazarTextoParticionado <- function(patron, reemplazo, texto_vector, n_partes = NULL) {
  patron = as.character(patron)
  reemplazo = as.character(reemplazo)
  modificaciones <- stats::setNames(reemplazo, patron)
  indices <- seq_along(texto_vector)
  if ( is.null(n_partes) ) {
    n_partes = 100 
  } else if ( is.integer(n_partes) & n_partes > 0 & n_partes <= length(texto_vector)  ) {
    n_partes
  } else {
    stop("n_pates debe ser un entero positivo mayor al tamaño del vector a modificar")
  }
  partes <- split(indices, cut(indices, breaks = n_partes, labels = FALSE))
  texto_modificado <- character()
  barraProgresoReinicio()
  for (parte in partes) {
    texto_modificado[parte] <-
      stringr::str_replace_all(texto_vector[parte], modificaciones)
    barraProgreso2(partes)
  }
  return(texto_modificado)
}

irregularidades_CODIGO_CUENTA <-
  consolidada %>%
  filter( grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% 
  distinct(CODIGO,CUENTA)

SB <-
  consolidada %>%
  filter( ! grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>%
  mutate( CODIGO = reemplazarTextoParticionado(c(100,300,400),c(1,2,3),CODIGO))

catalogo_CODIGO_SB <-
  consolidada %>%
  group_by(CODIGO, CUENTA) %>%
  summarise(CANTIDAD = n()) %>%
  filter( grepl("^[0-9]+$", CODIGO) ) %>%
  mutate( CODIGO = as.integer(CODIGO) ) %>%
  filter( CODIGO > 0 )

indices <- match(SB$CUENTA, catalogo_CODIGO_SB$CUENTA)

SB <- SB %>%
  mutate( CODIGO = ifelse(is.na(CODIGO), catalogo_CODIGO_SB$CODIGO[indices], CODIGO)) %>%
  filter( ! is.na(CODIGO) ) %>%
  mutate( CODIGO = as.integer(CODIGO) ) %>%
  mutate( CUENTA = 
            reemplazarTextoParticionado(
              c("^ACTIVO$","^TOTAL ACTIVO$","^TOTAL ACTIVOS$","^PASIVO$","^TOTAL PASIVO$","^TOTAL PASIVOS$","^TOTAL PATRIMONIO$","^TOTAL INGRESOS$"),
              c("ACTIVOS","ACTIVOS","ACTIVOS","PASIVOS","PASIVOS","PASIVOS","PATRIMONIO","INGRESOS"),
              CUENTA) )

SB %>% distinct(CODIGO, CUENTA) %>% View()
SB %>% filter(CODIGO == 1)

SB_ <- SB %>%
  group_by(FECHA, SEGMENTO, RUC, RAZON_SOCIAL, CODIGO, CUENTA) %>%
  filter(!(duplicated(VALOR) & is.na(VALOR))) %>%
  ungroup()

SB_ <- SB %>%
  group_by(FECHA, SEGMENTO, RUC, RAZON_SOCIAL, CODIGO, CUENTA) %>%
  filter(!all(is.na(VALOR))) %>%
  ungroup()

SB_ <- SB %>% sample_n(100000) %>%
  filter(!(duplicated(SB[, -which(names(SB) == "VALOR")]) & is.na(VALOR)))

SB_ %>% filter(CODIGO == 1) %>% View()


