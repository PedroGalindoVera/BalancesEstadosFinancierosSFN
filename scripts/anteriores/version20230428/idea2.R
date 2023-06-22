
indice_fila_nombres_columnas <- NULL
nombres_columnas <- NULL
for (ruta_libro in rutas_libros_seleccionados) {
  hoja <- readxl::read_excel(ruta_libro, sheet = "BALANCE", n_max = 20)
  indice_fila_nombres_columnas[ruta_libro] <- indicePrimeraFilDecimalTabla(hoja) - 1
  nombres_columna <- unname(unlist(hoja[indice_fila_nombres_columnas[ruta_libro],]))
  nombres_columnas <- c(nombres_columnas,nombres_columna)
}
length(indice_fila_nombres_columnas)
sort(unique(nombres_columnas))
