#' organizar documentos
#'
#' @param metadocs tibble lida com ler_metadocs_stj
#' @param docs docs
#' @param plano usar somente se for via bash
#' @param workers usar somente se for via bash
#'
#' @return tibble
#' @export
#'
stj_organizar_docs <- function(metadocs = NULL,docs = NULL,plano="sequencial",workers=1){


  metadocs %>%
    dplyr::select(registro=processo,sequencial,nome) %>%
    dplyr::mutate(registro = sub(registro,5,4,"/"),
                  registro = sub(registro,13,12,"-")) %>%
    dplyr::right_join(docs,by="sequencial") %>%
    dplyr::filter(stringr::str_detect(nome,"(?i)(relat.rio|voto|ementa|decis.o)")) %>%
    stats::na.omit() %>%
    tidyr::pivot_wider(id_cols=c("sequencial","registro"),names_from = "nome",
                       values_from = "documento") %>%
    #dplyr::select(-sequencial) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ano = stringr::str_extract(registro,"\\d+")) %>% # linha criada para eventual paralelização
    dplyr::group_by(ano) %>% # linha criada para eventual paralelização
    dplyr::group_split() %>%  # linha criada para eventual paralelização
    purrr::map_dfr(~{  # linha criada para eventual paralelização
      purrr::modify_depth(.x,2,~replace(.x,which(purrr::is_empty(.x)),NA_character_)) %>%
        tidyr::unnest()
    }) %>%
    tidyr::pivot_longer(docs,-c(registro,sequencial,ano),names_to = "documento",values_to="julgado") %>%
    dplyr::distinct(registro,sequencial,ano,.keep_all=TRUE)
}
