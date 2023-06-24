library(dplyr)
lista_SEPS <- generarListaTablasSEPS()
SEPS <- lista_SEPS %>% bind_rows() %>% distinct()
RAZON_SOCIAL <- SEPS %>% pull(RAZON_SOCIAL) %>% unique()
CUENTA <- SEPS %>% pull(CUENTA) %>% unique()

expresion_regular <-
  paste0("(?#negacion)[^",
         "(?#minusculas)a-z",
         "(?#minusculas tildadas)áéíóúñ",
         "(?#mayusculas)A-Z",
         "(?#mayusculas tildadas)ÁÉÍÓÚÑ",
         "(?#numeros)0-9",
         "(?#separadores) ()\"\\./,;–-",
         "]")

grep(expresion_regular, RAZON_SOCIAL, value = TRUE)
grep(expresion_regular, CUENTA, value = TRUE)

expresion_regular_buqueda <- "[ÁÉÍÓÚáéíóú]"
expresion_regular_correccion <- "[AEIOUaeiou]"
texto_vector <- CUENTA
corregido <- chartr(expresion_regular_buqueda, expresion_regular_correccion, texto_vector)

texto_vector <- CUENTA
buqueda  <- c("Á", "É", "Í", "Ó", "Ú", "á", "é", "í", "ó", "ú", "\\.")#, "–", " {2,}", "^\\s+", "\\s+$")
remplazo <- c("A", "E", "I", "O", "U", "a", "e", "i", "o", "u",  "")#, "-", " ", "", "")
correciones <- setNames(remplazo,buqueda)
texto_corregido <- str_replace_all(texto_vector, correciones)
control_cambios <-
  data.frame(original = texto_vector, modificado = texto_corregido) %>%
  mutate(
    #cambios = original != modificado,
    tildes = grepl("[ÁÉÍÓÚáéíóú]", original),
    separadores = grepl("[\"/.,;–-]", original),
    "\"" = grepl("\"", original),
    "/" = grepl("/", original),
    "punto" = grepl("\\.", original),
    "," = grepl(",", original),
    ";" = grepl(";", original),
    "–" = grepl("–", original),
    "-" = grepl("-", original),
    "mas_de_1_espacio" = grepl(" {2,}", original),
    "espacio_inicial" = grepl("^\\s+", original),
    "espacio_final" = grepl("\\s+$", original)
  )
  #filter(original != modificado)


expresion_regular_buqueda <- paste0(buqueda,collapse = "|")
grep(expresion_regular_buqueda,texto_corregido, value = TRUE)

grep(expresion_regular, texto_corregido, value = TRUE)

texto_corregido2 <- correcionCaracteresVectorizada(texto_corregido)

grep(expresion_regular, texto_corregido2, value = TRUE)

A <- data.frame("original" = texto_vector,
                "sin_tildes" = texto_corregido,
                "recodificacion" = texto_corregido2)
A$logico <- A$`original` == A$`sin_tildes`



