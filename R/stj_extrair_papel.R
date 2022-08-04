#' Extrai papeln da parte  no texto da decisão
#'
#' @param x Inteiro teor da decisão
#' @param padrao Padrão, parte buscaa,depois de dois pontos e
#'     espaço. Já ignora caixa.
#'
#' @return Papel da parte buscada
#' @export
#'
stj_extrair_papel <- function(x, padrao){

  regex <- paste0("(?i).+:\\s+",padrao)

  stringr::str_extract(x, regex) |>
    stringr::str_extract(".+?(?=:)") |>
    stringr::str_trim()

}
