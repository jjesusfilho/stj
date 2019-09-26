#' Organiza todos os documentos decisivos, colocando-os numa única observação
#'
#' @param metadocs retorno da função ler_metadocs_stj
#' @param docs   retorno da função ler_documentos_stj
#'
#' @details Se você quer manter documentos que não são decisórios,
#'     tais como certidões, não use esta função.
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- organizar_docs_stj(metadocs, docs)
#' }
organizar_docs_stj <- function(metadocs,docs){
  metadocs %>%
    dplyr::select(registro=processo,sequenciais,nome) %>%
    dplyr::mutate(registro = sub(registro,5,4,"/"),
                  registro = sub(registro,13,12,"-")) %>%
    dplyr::right_join(docs,by="sequencial") %>%
    dplyr::filter(stringr::str_detect(nome,"(?i)(relat.rio|voto|ementa|decis.o)")) %>%
    stats::na.omit() %>%
    dplyr::select(-sequenciais) %>%
    tidyr::spread("nome","documento") %>%
    janitor::clean_names()

}
