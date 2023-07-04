catalogo_OSFPS_ultimo <- catalogo_OSFPS %>%
  group_by(RUC, RAZON_SOCIAL, SEGMENTO) %>%
  slice_tail(n = 1) %>%
  ungroup()

catalogo_OSFPS_ultimo <- catalogo_OSFPS %>%
  group_by(RUC, RAZON_SOCIAL) %>%
  filter(FECHA == max(FECHA)) %>%
  ungroup()

SB %>% pull(CODIGO) %>% unique()


codigos_unicos <- SB %>% pull(CODIGO) %>% unique()

fechas_codigos <- SB %>%
  group_by(FECHA) %>%
  summarise(codigos = list(unique(CODIGO))) %>%
  mutate(faltantes = map(codigos, ~ setdiff(codigos_unicos, .x)))
fechas_codigos$n_discrepancias <-
  sapply(1:125,function(k) length(fechas_codigos$faltantes[[k]]))

fechas_codigos

SEPS %>% pull(CODIGO) %>% unique() %>% length()

catalogo_ <- catalogo_cuentas %>%
  group_by(CODIGO) %>%
  filter(FECHA == max(FECHA)) %>%
  slice_tail(n = 1) %>%
  ungroup()






