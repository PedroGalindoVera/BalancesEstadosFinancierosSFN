# Instalar y cargar el paquete readr
install.packages("readr")
library(readr)

# Usar la función guess_encoding para determinar la codificación del archivo
ruta <- "data/Fuente/SEPS/Bases de Datos/2015 EEFF MEN/2015.csv"
encoding <- readr::guess_encoding(ruta, n_max = 3000000)

# Ver los resultados
print(encoding)

data <- readr::read_csv(ruta, locale = locale(encoding = "ISO-8859-1"))

data <- read_delim(ruta, delim = ";", locale = locale(encoding = "ISO-8859-9"))

unique(data$RAZON_SOCIAL)

