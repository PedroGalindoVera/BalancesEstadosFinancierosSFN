
expresion_regular_buqueda <- "[ÁÉÍÓÚáéíóú]"
expresion_regular_correccion <- "[AEIOUaeiou]"
texto_vector <- unique(CUENTA)
corregido <- chartr(expresion_regular_buqueda, expresion_regular_correccion, texto_vector)

A <- data.frame(
  texto_vector, corregido
)
A$Logico <- A$texto_vector==A$corregido

grep(expresion_regular_buqueda,corregido)


buqueda <- c("Á", "É", "Í", "Ó", "Ú", "á", "é", "í", "ó", "ú", " {2,}", "^\\s+", "\\s+$")
remplazo <- c("A", "E", "I", "O", "U", "a", "e", "i", "o", "u", " ", "", "")
correciones <- setNames(remplazo,buqueda)
texto_corregido <- str_replace_all(texto_vector, correciones)

expresion_regular_buqueda <- paste0(buqueda,collapse = "|")
grep(expresion_regular_buqueda,texto_corregido, value = TRUE)

expresion_regular <-
  paste0("(?#negacion)[^",
         "(?#minusculas)a-z",
         "(?#minusculas tildadas)áéíóúñ",
         "(?#mayusculas)A-Z",
         "(?#mayusculas tildadas)ÁÉÍÓÚÑ",
         "(?#numeros)0-9",
         "(?#separadores) (),–/",
         "]")
grep(expresion_regular, texto_corregido, value = TRUE)

texto_corregido2 <- correcionCaracteresVectorizada(texto_corregido)

grep(expresion_regular, texto_corregido2, value = TRUE)

A <- data.frame("original" = texto_vector,
                "sin_tildes" = texto_corregido,
                "recodificacion" = texto_corregido2)
A$logico <- A$`original` == A$`sin_tildes`



