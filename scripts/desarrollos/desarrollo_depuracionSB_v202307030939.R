depurarCodigoCuentaSB <- function(consolidada) {
  
  requerirPaquetes("dplyr")
  
  irregularidades_CODIGO_CUENTA <-
    consolidada %>%
    filter( grepl("^200$|^500$|^600$|^700$|[[:alpha:]]|^0$|\\+|-",CODIGO) ) %>% 
    distinct(CODIGO,CUENTA)
  
  exportarReporteTabla(irregularidades_CODIGO_CUENTA,
                       "Irregularidades CODIGO CUENTA SB")
  
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
                CUENTA) ) %>%
    distinct()
  
  SB <- SB %>%
    group_by(FECHA, SEGMENTO, RUC, RAZON_SOCIAL, CODIGO, CUENTA) %>%
    filter(if (any(is.na(VALOR)) & n() > 1) !is.na(VALOR) else TRUE) %>%
    ungroup()
}

SB %>% filter(CODIGO == 1) %>% View()
