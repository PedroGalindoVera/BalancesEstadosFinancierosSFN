analisisVinculosPaginaWeb <- function() {
  if (!require("rvest")) {
    install.packages("rvest")
  }
  
  library(rvest)
  
  link <- "https://estadisticas.seps.gob.ec/index.php/estadisticas-sfps/"
  pagina <- rvest::read_html(link)
  
  contenedores_div <- 
    pagina %>% 
    rvest::html_nodes("div.vc_row.wpb_row.vc_row-fluid.vc_custom_1623732905705")
  
  nombre_link_referencia <-
    sapply(contenedores_div, function(x) grepl("1387", x))
  
  contenedor_div_BaseDatos <-
    contenedores_div[which(nombre_link_referencia)]
  
  links_BaseDatos <-
    rvest::html_nodes(contenedor_div_BaseDatos,"a") %>%
    rvest::html_attr("href")

  cat("\nVínculos rescatados de la página analizada:\n")
  print(links_BaseDatos)
  
  return(links_BaseDatos)
}