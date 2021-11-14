#' Ler fases dos processos do STJ
#'
#' @param arquivos Se os caminhos para os arquivos forem fornecidos
#'     o diretório é ignorado.
#' @param diretorio Diretório se arquivos não forem informados
#' @return tibble com fases do processo
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_fases_stj(diretorio = ".")
#' }
stj_ler_fases_classe <- function( arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(
      path = diretorio,
      pattern = ".html",
      full.names = T
    )

  }



  purrr::map_dfr(arquivos,  purrr::possibly(~{

    classe <- stringr::str_extract(.x, "(?<=classe_)[a-z]+")
    numero <- stringr::str_extract(.x, "(?<=numero_)\\d+")

    resposta <- xml2::read_html(.x)

    data_hora <-xml2::xml_find_all(resposta,"//*[@class='clsFaseDataHora']") %>%
      xml2::xml_text()

    fase <-xml2::xml_find_all(resposta,"//*[@class='classSpanFaseTexto']|//*[@class='classSpanFaseTexto clssSpanFaseTextoComLink']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble (classe = classe, numero = numero, data_hora, fase)

  },NULL))

}
