requerimientoPaquetes <- function() {
  paquetes <-
    c(
      "beepr",
      "data.table", "dplyr",
      "httr",
      "lubridate",
      "openxlsx",
      "parallel", "parsedate", "purrr",
      "readxl", "reshape2", "rlang", "rvest",
      "stats", "stringdist", "stringr",  
      "tools",
      "utils"
    )
  for (paquete in paquetes) {
    if ( !require(paquete, character.only = TRUE) ) {
      install.packages(paquete)
    }
  }
}
