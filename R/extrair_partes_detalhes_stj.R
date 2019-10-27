#' Extrai partes ou somente detalhes da tibble detalhes
#'
#' @param df tibble criada a partir da função ler_detalhes_stj
#' @param dados informar se "partes" ou "detalhes'
#'  Default: "partes"
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' partes <- extrair_partes_detalhes(df,dados="partes")
#' }
extrair_partes_detalhes_stj <- function(df = NULL, dados = c("partes","detalhes")){

  dados <- magrittr::extract(dados,1)

  detalhes<-c("PROCESSO:",
              "LOCALIZAÇÃO:", "TIPO:", "AUTUAÇÃO:", "NÚMERO ÚNICO:",
              "RELATOR(A):", "RAMO DO DIREITO:", "ASSUNTO(S):", "TRIBUNAL DE ORIGEM:",
              "NÚMEROS DE ORIGEM:", "", "ÚLTIMA FASE:",
              "T. ORIGEM :", "UF        :",  "A.CENTRAL :")


  if (dados == "partes"){

    dplyr::filter(df, !is.element(variavel,detalhes))


  } else {

    dplyr::filter(df, is.element(variavel,detalhes))


}
}
