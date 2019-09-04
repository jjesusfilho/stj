#' Ler detalhes dos processos do STJ
#'
#' @param diretorio Diretório se arquivos não forem informados
#' @param arquivos Se os caminhos para os arquivos forem fornecidos
#'     o diretório é ignorado.
#'
#' @return tibble com detalhes do processo
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_detalhes_stj(diretorio = ".")
#' }
ler_detalhes_stj <- function(diretorio = ".", arquivos = NULL){

  if (is.null(arquivos)){

    arquivos <- list.files(
      path = diretorio,
      pattern = ".html",
      full.names = T
    )

  }

  processos <- stringr::str_extract(arquivos,"(?<=stj_).+?(?=.html)") %>%
               stringr::str_replace("_","/")

  purrr::map2_dfr(arquivos, processos,  purrr::possibly(~{

    resposta <- xml2::read_html(.x)

    variavel <-xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesLabel']") %>%
       xml2::xml_text(trim=T)

    valor <-xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesTexto']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble (registro_stj = .y,  variavel, valor)

  },NULL)) %>%
        dplyr::group_by_at(dplyr::vars(-valor)) %>%
        dplyr::mutate(row_id = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        tidyr::spread(key = variavel, value = valor) %>%
        dplyr::select(-row_id) %>%
    janitor::clean_names()

}
