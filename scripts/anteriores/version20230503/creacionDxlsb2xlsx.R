xlsb2xlsx <- function(ruta_archivo_xlsb) {
  
  # Esta función permite transformar el formato de un archivo de Excel con extención ".xlsb" a ".xlsx" y reemplazarlo empleando Windows PowerShell
  
  # EJEMPLO:
  # ruta_archivo_xlsb <- "data/Fuente/Casos Particulares/BOL_FIN_PUB_SEPT_20.xlsb"
  # xlsb2xlsx(ruta_archivo_xlsb)
  
  verificadorExcel <- function() {
    
    # Esta función verifica la instalación de Excel
    
    # Script de Power Shell para verificar la instalación de Excel
    script <- paste(
      "$excelPath = Get-ItemProperty \"HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe\" -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Path;",
      #if ($excelPath) { 'installed' } else { 'not installed' }",
      "if ($excelPath) { $true } else { $false }",
      sep = "\n"
    )
    # Guardar el script en un archivo en el directorio principal
    writeLines(script, "temporal.ps1")
    # Ejecutar el script de PowerShell y capturar el resultado
    result <- system("powershell -File temporal.ps1", intern = TRUE)
    # Eliminar el archivo de script
    file.remove("temporal.ps1")
    # Devolver el resultado como un valor lógico en R
    return(as.logical(result))
  }
  verificadorArchivo <- function(ruta_archivo_xlsb) {
    if (file.exists(ruta_archivo_xlsb)) {
      return(TRUE)
    } else {
      cat(paste("\nLa ruta no existe\n"))
      return(FALSE)
    }
  }
  verificadorFormato <- function(ruta_archivo_xlsb) {
    # Requerimiento de paquetes
    if (!require("tools")) {
      install.packages("tools")
      library(tools)
    }
    
    if (tools::file_ext(ruta_archivo_xlsb) == "xlsb") {
      return(TRUE)
    } else {
      cat(paste("\nEl archivo con ruta: [", ruta_archivo_xlsb,"],",
                "no es de formato \".xlsb\" por lo que no se realizaron cambios\n"))
      return(FALSE)
    }
    
  }
  
  # Condiciones no admisibles
  if (!verificadorExcel() || !verificadorArchivo(ruta_archivo_xlsb) || !verificadorFormato(ruta_archivo_xlsb)) {
    break
  }
  
  # Ruta del archivo xlsb
  xlsbFile <- normalizePath(ruta_archivo_xlsb)
  
  # Ruta del archivo xlsx
  xlsxFile <- normalizePath(gsub(".xlsb", ".xlsx",ruta_archivo_xlsb))
  
  # Crear el script de PowerShell
  script <- paste(
    # Ruta del archivo xlsb
    paste0("$xlsbFile = ", '"', xlsbFile, '"'),
    # Ruta del archivo xlsx
    paste0("$xlsxFile = ", '"', xlsxFile, '"'),
    # Crear un objeto COM de Excel
    "$excel = New-Object -ComObject Excel.Application",
    # Deshabilitar las alertas
    "$excel.DisplayAlerts = $false",
    # Abrir el archivo xlsb
    "$workbook = $excel.Workbooks.Open($xlsbFile)",
    # Guardar como archivo xlsx, donde 1 corresponde al formato xls y 51 a xlsx
    "$workbook.SaveAs($xlsxFile, 51)",
    # Cerrar el libro y salir de Excel
    "$workbook.Close()",
    "$excel.Quit()",
    sep = "\n"
  )
  
  # Guardar el script en un archivo en el directorio principal
  writeLines(script, "convert.ps1")
  
  # Mensaje
  cat(paste("\nSe remplazo el archivo \".xlsb\" de ruta: [", xlsbFile,"],",
            "con el archivo \".xlsx\" de ruta [", xlsxFile,"]\n"))

  # Ejecutar el script de PowerShell
  shell("powershell -File convert.ps1", wait = TRUE)

  # Eliminar el archivo de script y el .xlsb original
  file.remove("convert.ps1",xlsbFile)
}

