library(Microsoft365R)

od <- get_personal_onedrive()
od$list_files()

od$upload_file("somedata.xlsx")

ruta_archivo <- "\\\\192.168.10.244\\inteligencia\\SIEVA SEPS Mensual\\Balance SIEVA SEPS 2023-04-30.xlsx"
file.exists(ruta_archivo)
ruta_destino <- "Documentos/Balance SIEVA SEPS 2023-04-30.xlsx"
od$upload_file(ruta_archivo, dest = ruta_destino)

od$upload_file(ruta_archivo)

od$upload_file("Balance SIEVA SEPS 2023-04-30.xlsx")



archivo_local <- "Balance SIEVA SEPS 2023-04-30.xlsx"
file.exists(archivo_local)
ruta_destino <- "Documentos/Balance SIEVA SEPS 2023-04-30.xlsx"
od$upload_file(archivo_local, dest = ruta_destino)
