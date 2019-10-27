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
      xml2::xml_text(trim=T) %>%
      dplyr::na_if("_         :")

    p <- 2:(stringr::str_which(variavel,"LOCALIZA")-1)

    parte <- vector(length=length(variavel)) %>%
      replace(p,TRUE)

    valor <- xml2::xml_find_all(resposta,"//*[@class='classSpanDetalhesTexto']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble(numero = numero, registro = registro, parte, variavel = variavel, valor = valor) %>%
      tidyr::fill(variavel)

  },NULL))
}
