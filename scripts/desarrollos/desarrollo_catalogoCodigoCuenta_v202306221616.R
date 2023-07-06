correcionCaracteresVectorizada<- function(vector_texto) {
  
  requerirPaquetes("parallel","stats","stringr")
  
  analisis_caracteres <- analisisCaracteresIncorrectos(vector_texto)$caracter
  cat("\n\n\033[1mLista de caracteres a corregir:\033[0m\n")
  print(analisis_caracteres)
  caracter_incorrecto <- c("  ", analisis_caracteres$original)
  caracter_correcto <- c(" ", analisis_caracteres$identificado)
  correcciones <- stats::setNames(caracter_correcto, caracter_incorrecto)
  cat("\n\033[1;32mCorreción de caracteres vectorizada...\033[0m\n")
  texto_corregido <- stringr::str_replace_all(vector_texto, correcciones)
  return(texto_corregido)
}

lista_SEPS <- generarListaTablasSEPS()

SEPS <- lista_SEPS %>% bind_rows() %>% distinct()

CUENTA_unica <- SEPS %>% reframe(CUENTA = unique(CUENTA))
a <- analisisCaracteresIncorrectos(CUENTA_unica$CUENTA)
CUENTA_unica$CORRECCION <- correcionCaracteresParalelizada(CUENTA_unica$CUENTA)

CODIGO_unica <- SEPS %>% reframe(CODIGO = unique(CODIGO))

catalogo <-
  lista_SEPS %>%
  bind_rows() %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA))

catalogo_fecha <-
  lista_SEPS %>%
  bind_rows() %>%
  group_by(CODIGO) %>%
  reframe(CUENTA = unique(CUENTA),
          FECHA = paste(unique(FECHA), collapse = ', ')) %>%
  select(CUENTA, CODIGO, FECHA)


a<-correcionCaracteresParalelizada(catalogo$CUENTA)
data.frame(catalogo$CUENTA,a) %>% View()


vector_texto <- CUENTA_unica$CUENTA

A <- 
  tibble(
    original = vector_texto,
    correccion = correcionCaracteresParalelizada(vector_texto)
  ) %>%
  mutate(coinciden = original == correccion)

vector_texto <-
  SEPS %>%
  select(CUENTA)

vector_corregido_v <- system.time(correcionCaracteresVectorizada(vector_texto$CUENTA))

vector_corregido_p <- system.time({correcionCaracteresParalelizada(vector_texto$CUENTA)})

vector_texto <-
  SEPS %>% #head(1000000) %>%
  select(CUENTA) %>%
  mutate(
    CUENTA = CUENTA,
    CORREGIDA = correcionCaracteresVectorizada(CUENTA),
    COINCIDENCIA = CUENTA == CORREGIDA
  )

catalogo <-
  tabla_concatenada %>%
  group_by(CODIGO) %>%
  reframe(CUENTA_CORREGIDA = unique(CUENTA_CORREGIDA))

