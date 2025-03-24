#' Baixa comunicações do STJ via api do CNJ
#'
#' @param processo Número único do processo
#' @param dt_inicio Data do início
#' @param diretorio Data final
#'
#' @returns json
#' @export
#'
cnj_consultar_comunicacao1 <- function(processo = NULL,
                                       dt_inicio = NULL,
                                       dt_fim = NULL,
                                       diretorio = ".") {
  dt_inicio = lubridate::dmy(dt_inicio)

  dt_fim = lubridate::dmy(dt_fim)

  processo <- stringr::str_remove_all(processo, "\\D+")

  if (nchar(processo) != 20) {
    stop("O número do processo tem de ter 20 dígitos")

  }

  uri_parseada <- structure(
    list(
      scheme = "https",
      hostname = "comunicaapi.pje.jus.br",
      username = NULL,
      password = NULL,
      port = NULL,
      path = "/api/v1/comunicacao",
      query = list(
        pagina = "1",
        itensPorPagina = "1000",
        siglaTribunal = "STJ",
        dataDisponibilizacaoInicio = dt_inicio,
        dataDisponibilizacaoFim = dt_fim,
        numeroProcesso = processo
      ),
      fragment = NULL
    ),
    class = "httr2_url"
  )

  uri <- httr2::url_build(uri_parseada)

  basename <- glue::glue("cnj_processo_{processo}_dt_inicio_{dt_inicio}_dt_fim_{dt_fim}.json") |>
    stringr::str_replace_all("-", "_")

  arquivo <- file.path(diretorio, basename)

  uri |>
    httr2::request() |>
    httr2::req_perform(path = arquivo)

}
