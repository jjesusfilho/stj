#' Extrai dispositivo da decisão
#'
#' @param x Inteiro teor da decisão
#'
#' @return character
#' @export
#'
stj_extrair_dispositivo <- function(x){
  x <- dplyr::case_when(
    stringr::str_detect(x, "a vista de tais pressupostos") ~ stringr::str_extract(x, "a vista de tais pressupostos\\X+"),
    stringr::str_detect(x, 'nte o exposto') ~ stringr::str_extract(x, "exposto(?!.*exposto)\\X+"),
    stringr::str_detect(x,"a vista do exposto") ~ stringr::str_extract(x,"exposto(?!.*exposto)\\X+"),
    stringr::str_detect(x, "por tais raz.es") ~ stringr::str_extract(x, "por tais raz.es(?!.*por tais raz.es)\\X*"),
    stringr::str_detect(x, "tal o contexto") ~ stringr::str_extract(x,"tal o contexto\\X+"),
    stringr::str_detect(x, "exposto,") ~ stringr::str_extract(x,"exposto,(?!.*exposto,)\\X+"),
    stringr::str_detect(x, "assim,") ~ stringr::str_extract(x, "assim,(?!.*assim,)\\X+"),
    TRUE  ~  stringr::str_sub(x, -1000)
  )
  x
}
