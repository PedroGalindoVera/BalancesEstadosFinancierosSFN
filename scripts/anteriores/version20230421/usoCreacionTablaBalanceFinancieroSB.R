source("scripts/herramientasImportacionModificacionTablasSB.R")

compiladorHojasBalanceFinancieroSB()

library(readr)
BalanceFianciero <- read_csv("data/Base de Datos/SBP Boletin Financiero 2023-02-28.csv")
