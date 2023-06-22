library(dplyr)
result <- tabla_combinada %>%
  filter(grepl("jardin azuayo", `Razón Social`, ignore.case = TRUE))


install.packages("readxl")
library(readxl)

# Read an Excel file into R
my_data <- read_excel("data/2021-EEFF-MEN/Boletin Financiero Segmento 1_ Diciembre_2021.xlsm", sheet = "ESTADO FINANCIERO")


nombres <- unique(tabla_combinada$`Razón Social`)

nombre_cooperativa <- grep("jardin", nombres, ignore.case = TRUE, value = TRUE)

result <- tabla_combinada %>%
  filter(grepl(nombre_cooperativa, `Razón Social`, ignore.case = TRUE))


tabla_combinada %>%
  filter(grepl("COOPERATIVA DE AHORRO Y CREDITO JARDIN AZUAYO LIMITADA", `Razón Social`, ignore.case = TRUE))



library(data.table)
mi_dt <- as.data.table(tabla_combinada)
setindex(mi_dt, `Razón Social`)
mi_dt[`Razón Social` == "COOPERATIVA DE AHORRO Y CREDITO JARDIN AZUAYO LIMITADA"]




library(lubridate)
unique(year(tabla_combinada$`Fecha de Corte`))
