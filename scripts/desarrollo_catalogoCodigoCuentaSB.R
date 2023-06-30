C <- consolidada %>% distinct(CODIGO,CUENTA)

SB <- depurar(consolidada)
CC <- SB %>% distinct(CODIGO,CUENTA)

catalogo_CODIGO <- SB_ %>% distinct(CODIGO,CUENTA) %>%
  mutate(NUM_DIGITOS = nchar(CODIGO)) %>%
  filter(! is.na(CODIGO) )

