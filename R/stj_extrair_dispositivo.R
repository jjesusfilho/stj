#' Extrai dispositivo da decisão
#'
#' @param x Inteiro teor da decisão
#'
#' @return character
#' @export
#'
stj_extrair_dispositivo <- function(x){
  x <- dplyr::case_when(
    stringr::str_detect(x, "a vista de tais pressupostos") ~ str_extract(x, "a vista de tais pressupostos.+"),
    stringr::str_detect(x, 'nte o exposto') ~ stringr::str_extract(x, "exposto(?!.*exposto).+"),
    stringr::str_detect(x,"a vista do exposto") ~ stringr::str_extract(x,"exposto(?!.*exposto).+"),
    stringr::str_detect(x, "por tais razoes") ~ stringr::str_extract(x, "por tais razoes(?!.*por tais razoes).*"),
    stringr::str_detect(x, "tal o contexto") ~ stringr::str_extract(x,"tal o contexto.+"),
    str_detect(x, "exposto,") ~ str_extract(x,"exposto,(?!.*exposto,).+"),
    stringr::str_detect(x, "assim,") ~ stringr::str_extract(x, "assim,(?!.*assim,).+"),
    TRUE  ~  stringr::str_sub(x, -1000)
  )
  x
}
