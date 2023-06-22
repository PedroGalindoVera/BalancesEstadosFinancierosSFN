SEPS_respaldo <- SEPS
SEPS <- SEPS_respaldo

SEPS <- correccionCaracteresParalelizadaRazonSocialSEPS(SEPS)

tabla <- SEPS
tabla <- SEPS_respaldo
nombres <- sort(unique(tabla$RAZON_SOCIAL))
nombres[grepl("[^[:alpha:] -./]", nombres)]

ruc <- sort(unique(SEPS$RUC))

tabla <- SEPS
tabla <- SEPS_respaldo
indice_coincidencia_ruc <- which(tabla$RUC == ruc[1])
tabla$RAZON_SOCIAL[indice_coincidencia_ruc]
unique(tabla$RAZON_SOCIAL[indice_coincidencia_ruc])



library(dplyr)
t <- tabla %>%
  group_by(RUC) %>%
  summarise(RAZON_SOCIAL = list(unique(RAZON_SOCIAL)),
            RAZON_SOCIAL_LENGTH = lengths(RAZON_SOCIAL))
  
t[which(t$RUC == "0390027923001"),]$RAZON_SOCIAL