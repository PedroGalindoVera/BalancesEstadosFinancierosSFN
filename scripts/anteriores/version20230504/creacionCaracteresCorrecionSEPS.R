nombres <- sort(unique(SEPS$RAZON_SOCIAL))

prueba <- grepl("[^[:alpha:][:digit:] .,;/-]",nombres)
sum(prueba)
nombres[prueba]

correciones <- c("Ã‘" = "Ñ", "Ã‰" = "É", "Ã\u008d" = "Í", "Ã“" = "Ó")
# correciones <- c("AÃ‘A" = "AÑA", "AÃ‘O" = "AÑO", "EÃ‘A" = "EÑA", "EÃ‘O" = "EÑO",
#                  "IÃ‘A" = "IÑA", "UÃ‘O" = "UÑO", "YÃ‘A" = "YÑA", "RÃ‰D" = "RÉD",
#                  "IÃ“N" = "IÓN", "RÃ\u008dO" = "RÍO")

nombres_mod <- stringr::str_replace_all(nombres,correciones)

prueba <- grepl("[^[:alpha:][:digit:] .,;/-]",nombres_mod)
sum(prueba)
nombres_mod[prueba]

data.frame(nombres_mod[prueba])

nom <- data.frame(nombres_mod)

#
subtabla <-  head(SEPS,2000000)
prueba <- grepl("[^[:alpha:][:digit:] .,;/-]",subtabla$RAZON_SOCIAL)
sum(prueba)
View(subtabla[prueba,])
correciones <- c("Ã‘" = "Ñ", "Ã‰" = "É", "Ã\u008d" = "Í", "Ã“" = "Ó")
subtabla$RAZON_SOCIAL <- stringr::str_replace_all(subtabla$RAZON_SOCIAL,correciones)
prueba <- grepl("[^[:alpha:][:digit:] .,;/-]",subtabla$RAZON_SOCIAL)
sum(prueba)
View(subtabla[prueba,])


# Empleando codificiacion ----

texto <- "COOPERATIVA DE AHORRO Y CREDITO DE LA PEQUEÃ‘A EMPRESA BIBLIAN LIMITADA"
texto_limpio <- iconv(texto, from = "Windows-1252", to = "UTF-8")
texto_limpio