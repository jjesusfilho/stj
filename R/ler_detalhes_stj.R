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
      pattern = "_\\d{2}_(processo|registro)",
      full.names = T
    )

  }



  purrr::map_dfr(arquivos,  purrr::possibly(~{

    numero <- stringr::str_extract(.x,"(?<=stj_).+?(?=.html)") %>%
      stringr::str_remove_all("\\D+")

    resposta <- xml2::read_html(.x)

    registro <- resposta %>%
      xml2::xml_find_first("//span[@id='idSpanNumeroRegistro']") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_remove_all("(\\(|\\))")

    variavel <-xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesLabel']") %>%
      xml2::xml_text(trim=T)

    valor <- xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesTexto']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble(numero = numero, registro = registro,  variavel = variavel, valor = valor)

  },NULL)) %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names()

}
