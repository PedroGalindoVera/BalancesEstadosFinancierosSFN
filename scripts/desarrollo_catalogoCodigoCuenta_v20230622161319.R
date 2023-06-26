lista_SEPS <- generarListaTablasSEPS()

k <- 1
codigo <- 1
lista_SEPS[[k]]$CUENTA[lista_SEPS[[k]]$CODIGO == codigo] %>% unique()

length(unique(lista_SEPS[[k]]$CODIGO))

length(unique(lista_SEPS[[k]]$CUENTA))

# HAY MUCHOS MAS CODIGOS QUE CUENTAS
# > length(unique(lista_SEPS[[k]]$CODIGO))
# [1] 1523
# > length(unique(lista_SEPS[[k]]$CUENTA))
# [1] 732

resumen <-
  data.frame(
    ARCHIVO = names(lista_SEPS),
    CODIGOS = sapply(lista_SEPS, function(df) length(unique(df))))

catalogo <-
  lista_SEPS[[k]] %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = unique(CUENTA)) %>%
  ungroup() %>%
  group_by(CUENTA) %>%
  summarise(CODIGO = list(unique(CODIGO)),
            NUM_CUENTAS = n())

catalogo_cue_cod <-
  lista_SEPS[[k]] %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = list(unique(CUENTA))) %>%
  ungroup() %>%
  group_by(CUENTA) %>%
  summarise(CODIGO = list(unique(CODIGO)),
            NUM_CUENTAS = n())

catalogo_cod_cue <-
  lista_SEPS[[k]] %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = list(unique(CUENTA)),
            NUM_CUENTAS = n())  

catalogo <-
  SEPS[1:10000,] %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = unique(CUENTA)) %>%
  ungroup() %>%
  group_by(CUENTA) %>%
  summarise(CODIGO = list(unique(CODIGO)),
            NUM_CUENTAS = n())

SEPS_sampled <- SEPS %>% sample_n(1000000)

catalogo <-
  SEPS_sampled %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = unique(CUENTA)) %>%
  ungroup() %>%
  group_by(CUENTA) %>%
  summarise(CODIGO = list(unique(CODIGO)),
            NUM_CUENTAS = n())

# ----
catalogo_fecha <-
  lista_SEPS[[k]] %>%
  group_by(CODIGO) %>%
  summarise(CUENTA = unique(CUENTA)) %>%
  select(CUENTA,CODIGO)

catalogo_fecha <-
  lista_SEPS[[k]] %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA), FECHA = unique(FECHA)) %>%
  select(CUENTA, CODIGO, FECHA)

catalogo_fecha <-
  lista_SEPS[c(1,2)] %>%
  bind_rows() %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA), FECHA = unique(FECHA)) %>%
  select(CUENTA, CODIGO, FECHA)

catalogo_fecha <-
  lista_SEPS %>%
  bind_rows() %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA))

# ----

catalogo_CODIGO_CUENTA <-
  SEPS %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA)) %>%
  add_count(CUENTA)

catalogo_CUENTA_CODIGO <-
  SEPS %>%
  group_by(CUENTA) %>%
  reframe(CODIGO = unique(CODIGO)) %>%
  add_count(CODIGO)

catalogo1 <-
  # SEPS %>%
  SEPS_estandar %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA),
          `Última Fecha de Uso` = max(FECHA)) %>%
  add_count(CODIGO) %>%
  rename(`NUMERO DE CUENTAS POR CODIGO` = n) %>%
  add_count(CUENTA) %>%
  rename(`NUMERO DE CODIGOS POR CUENTA` = n)

catalogo2 <-
  # SEPS %>%
  SEPS_estandar %>%
  group_by(CUENTA) %>%
  reframe(CODIGO = unique(CODIGO),
          `ÚLTIMA FECHA DE USO` = max(FECHA)) %>%
  add_count(CODIGO) %>%
  rename(`NUMERO DE CUENTAS POR CODIGO` = n) %>%
  add_count(CUENTA) %>%
  rename(`NUMERO DE CODIGOS POR CUENTA` = n) %>%
  select("CODIGO", "CUENTA", "ULTIMA FECHA DE USO",
         "NUMERO DE CUENTAS POR CODIGO", "NUMERO DE CODIGOS POR CUENTA")

catalogoCodigoCuenta <- function(tabla_balance_financiero) {
  requerirPaquetes("dplyr")
  catalogo <-
    tabla_balance_financiero %>%
    group_by(CUENTA) %>%
    reframe(CODIGO = unique(CODIGO),
            SEGMENTO = paste(sort(unique(SEGMENTO)), collapse = ", "),
            `PRIMERA FECHA DE USO` = min(FECHA),
            `ULTIMA FECHA DE USO` = max(FECHA)) %>%
    add_count(CODIGO) %>%
    rename(`NUMERO DE CUENTAS POR CODIGO` = n) %>%
    add_count(CUENTA) %>%
    rename(`NUMERO DE CODIGOS POR CUENTA` = n) #%>%
  # select("CODIGO", "CUENTA", "ULTIMA FECHA DE USO",
  #        "NUMERO DE CUENTAS POR CODIGO", "NUMERO DE CODIGOS POR CUENTA")
  return(catalogo)
}

catalogo <-
  SEPS_estandar %>%
  group_by(CODIGO, CUENTA, SEGMENTO) %>%
  reframe(`PRIMERA FECHA DE USO` = min(FECHA),
          `ULTIMA FECHA DE USO` = max(FECHA)) %>%
  add_count(CODIGO) %>%
  rename(`NUMERO DE CUENTAS POR CODIGO` = n) %>%
  add_count(CUENTA) %>%
  rename(`NUMERO DE CODIGOS POR CUENTA` = n) 

catalogo <-
  SEPS_estandar %>%
  distinct(CODIGO, CUENTA, SEGMENTO, FECHA)



