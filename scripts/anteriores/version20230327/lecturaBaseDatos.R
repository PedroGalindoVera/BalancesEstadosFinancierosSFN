library(DBI)

# Establece la conexión a la base de datos
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")

# Obtener una lista de las tablas en la base de datos
tablas <- DBI::dbListTables(con)

# Imprimir la lista
print(tablas)

# Lee la tabla "tabla[1]" desde la base de datos y guárdala en un data frame
datos <- DBI::dbReadTable(con, tablas[126])

# Cierra la conexión a la base de datos
DBI::dbDisconnect(con)