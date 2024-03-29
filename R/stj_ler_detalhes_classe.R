#' Ler detalhes dos processos do STJ baixados com stj_baixar_classe_processo
#'
#' @param arquivos Se os caminhos para os arquivos forem fornecidos
#'     o diretório é ignorado.
#' @param diretorio Diretório se arquivos não forem informados
#' @param wide Colocar no formato largo? Padrão é longo.
#'
#' @return tibble com detalhes do processo
#' @export
#'
#' @examples
#' \dontrun{
#' df <- stj_ler_detalhes_classe(diretorio = ".")
#' }
stj_ler_detalhes_classe <- function(arquivos = NULL, diretorio = ".", wide = FALSE){

  if (is.null(arquivos)){

    arquivos <- list.files(
      path = diretorio,
      pattern = "classe",
      full.names = T
    )

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  dados <-  purrr::map_dfr(arquivos,  purrr::possibly(~{

    pb$tick()


    classe <- stringr::str_extract(.x, "(?<=classe_)[a-z]+")
    numero <- stringr::str_extract(.x, "(?<=numero_)\\d+")


    resposta <- xml2::read_html(.x)

    registro <- resposta %>%
      xml2::xml_find_first("//span[@id='idSpanNumeroRegistro']") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_remove_all("(\\(|\\))")

    variavel <-xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesLabel']") %>%
      xml2::xml_text(trim=T) %>%
      dplyr::na_if("_         :")


    valor <- xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesTexto']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble(classe, numero = numero, registro = registro,  variavel = variavel, valor = valor) %>%
      tidyr::fill(variavel)

  },NULL))

  if (wide == TRUE) {
    dados <- dados %>% dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id)
  }

  return(dados)
}
