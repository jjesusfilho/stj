#' Ler fases dos processos do STJ
#'
#' @param diretorio Diretório se arquivos não forem informados
#' @param arquivos Se os caminhos para os arquivos forem fornecidos
#'     o diretório é ignorado.
#'
#' @return tibble com fases do processo
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_fases_stj(diretorio = ".")
#' }
stj_ler_fases <- function(diretorio = ".", arquivos = NULL){

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

    datahora <-xml2::xml_find_all(resposta,"//*[@class='clsFaseDataHora']") %>%
      xml2::xml_text()

    data <- stringr::str_sub(datahora,1,10) %>%
      lubridate::dmy()

    hora <- stringr::str_sub(datahora,11)

    fase <-xml2::xml_find_all(resposta,"//*[@class='classSpanFaseTexto']|//*[@class='classSpanFaseTexto clssSpanFaseTextoComLink']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble (registro_stj = .y,  data, hora, fase)

  },NULL)) %>%
    dplyr::mutate(hora = lubridate::hm(hora)) # não sei porque, mas só funciona com mutate

}
