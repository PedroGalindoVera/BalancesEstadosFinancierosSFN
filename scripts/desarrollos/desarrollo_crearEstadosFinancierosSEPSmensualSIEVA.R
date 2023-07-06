crearEstadosFinancierosSEPSsievaMensual <- function(data_frame) {
  requerirPaquetes("dplyr")
  
  ultima_fecha <- max(SEPS$FECHA)
  
  SEPS_SIEVA_mensual <-
    SEPS %>%
    filter(
      FECHA == ultima_fecha,
      SEGMENTO %in% c("MUTUALISTA", "SEGMENTO 1", "SEGMENTO 2", "SEGMENTO 3")) %>%
    mutate(FECHA = format(FECHA, "%d/%m/%Y")) %>%
    select(SEGMENTO, CODIGO, VALOR, FECHA, RUC) %>%
    distinct() %>%
    rename(
      segmento = SEGMENTO,
      cuenta = CODIGO,
      valor = VALOR,
      `fecha (dd/mm/AAAA)` = FECHA,
      ruc = RUC
    )
  
  ruta_dir_compartida <- "\\\\192.168.10.244\\inteligencia"
  exportarResultadosCSV(SEPS_SIEVA_mensual,"Balance SIEVA SEPS",ruta_dir_compartida)
}
