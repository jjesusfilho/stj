#' Lê arquivos baixados com stj_baixar_por_autuacao
#'
#' @param arquivos Vetor de csvs
#' @param diretorio Informar diretorio se omitir arquivos
#'
#' @return Tibble
#' @export
#'
stj_ler_por_autuacao <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern  = "csv$")

  }

pb <- progress::progress_bar$new(total = length(arquivos))

purrr::map_dfr(arquivos, purrr::possibly(~{

  pb$tick()

 data.table::fread(.x, colClasses = "character" )


},NULL)) |>
  janitor::clean_names()

}
