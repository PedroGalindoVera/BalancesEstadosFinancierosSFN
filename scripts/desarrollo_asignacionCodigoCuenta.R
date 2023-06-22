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
