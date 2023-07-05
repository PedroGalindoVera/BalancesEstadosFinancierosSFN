SB <- consolidada %>%
  filter( ! grepl("-",CODIGO) ) %>% # consolidada %>% filter( grepl("-",CODIGO) ) %>% View()
  filter( ! grepl("\\+",CODIGO) ) %>% # consolidada %>% filter( grepl("\\+",CODIGO) ) %>% View()
  filter( ! grepl("^0$",CODIGO) ) %>%  # consolidada %>% filter( grepl("^0$",CODIGO) ) %>% View()
  #filter( ! ( CODIGO == "ACTIVO" & CUENTA == "ACTIVO" & is.na(VALOR) ) ) %>% # consolidada %>% filter( CODIGO == "ACTIVO" & CUENTA == "ACTIVO" & is.na(VALOR) ) %>% View()
  #filter( ! grepl("^[[:alpha:]]$",CODIGO) ) %>% # consolidada %>% filter( grepl("^[[:alpha:]]$",CODIGO) ) %>% View()
  #filter( ! grepl("PRUEBA DE CUADRE",CODIGO) ) %>% # consolidada %>% filter( grepl("PRUEBA DE CUADRE",CODIGO) ) %>% View()
  filter( ! grepl("[[:alpha:]]",CODIGO) ) # consolidada %>% filter( grepl("[[:alpha:]]",CODIGO) ) %>% View()

catalogo_CODIGO_SB <- consolidada %>% distinct(CODIGO, CUENTA) %>% 
  mutate( CODIGO = as.integer(CODIGO) ) %>%
  filter( CODIGO > 0 )

indices <- match(SB$CUENTA, catalogo_CODIGO_SB$CUENTA)
SB$CODIGO[is.na(SB$CODIGO)] <-
  catalogo_CODIGO_SB$CODIGO[indices][is.na(SB$CODIGO)]

SB <- SB %>% filter( ! is.na(CODIGO) ) # SB %>% filter( is.na(CODIGO) ) %>% View()
  
SB <- SB %>%
  mutate(CODIGO = gsub("^100$","1",CODIGO)) %>%
  filter( ! grepl("200",CODIGO) ) %>% # SB %>% filter( grepl("^200$",CODIGO) ) %>% View()
  mutate(CODIGO = gsub("^300$","2",CODIGO), # SB %>% filter( grepl("^300$",CODIGO) ) %>% View()
         CODIGO = gsub("^400$","3",CODIGO)) %>%
  filter( ! grepl("500",CODIGO) ) %>%
  filter( ! grepl("600",CODIGO) ) %>%
  filter( ! grepl("700",CODIGO) )

# ----

SB %>% distinct(CODIGO,CUENTA) %>% View()

consolidada %>% filter( is.na(CODIGO) ) %>% View()

SB <- SB %>% mutate(CODIGO_ = as.integer(CODIGO))
indices <- match(SB$CUENTA, catalogo_CODIGO_SB$CUENTA)
#SB$CODIGO_ <- catalogo_CODIGO_SB$CODIGO[indices]
#SB$CODIGO_[is.na(SB$CODIGO)] <- catalogo_CODIGO_SB$CODIGO[indices][is.na(SB$CODIGO)]
SB %>% distinct(CODIGO,CODIGO_,CUENTA) %>% mutate(CODIGO == CODIGO_) %>% View()
SB %>% distinct(CODIGO,CODIGO_,CUENTA) %>% filter(is.na(CODIGO)) %>% View()


SB$CODIGO[is.na(SB$CODIGO)] <- catalogo_CODIGO_SB$CODIGO[indices][is.na(SB$CODIGO)]


SB %>% filter( grepl("^[[:alpha:]]$",CODIGO) ) %>% View()


