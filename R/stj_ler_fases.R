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

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    registro <- stringr::str_extract(.x,"(?<=stj_).+?(?=.html)") |>
               stringr::str_remove_all("\\D+")

    resposta <- xml2::read_html(.x)

    data <-xml2::xml_find_all(resposta,"//*[@class='clsFaseDataHora']") %>%
      xml2::xml_text() |>
      lubridate::dmy_hm(tz = "America/Sao_Paulo")

    fase <-xml2::xml_find_all(resposta,"//*[@class='classSpanFaseTexto']|//*[@class='classSpanFaseTexto clssSpanFaseTextoComLink']") %>%
      xml2::xml_text(trim=T)

    tibble::tibble (registro_stj = registro,  data,  fase)

  },NULL))

}
