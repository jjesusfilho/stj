#' Baixa processos do stj pelo número do registro
#'
#' @param registro_stj Número do registro
#' @param diretorio Diretório. Default para atual
#'
#' @return html baixado no diretório indicado
#' @export
#'
#' @examples
#' \dontrun{
#' baxiar_processo_stj("2016/0074331-0")
#'
#' }
baixar_processo_stj <- function(registro_stj = NULL, diretorio = ".") {

  url <- "https://ww2.stj.jus.br/processo/pesquisa/"

  body <-
    list(
      aplicacao = "processos.ea",
      acao = "pushconsultarprocessoconsultalimitenaoatendidasjaincluidas",
      descemail = "",
      senha = "",
      totalRegistrosPorPagina = "40",
      tipoPesquisaSecundaria = "",
      sequenciaisParteAdvogado = "-1",
      refinamentoAdvogado = "",
      refinamentoParte = "",
      tipoOperacaoFonetica = "",
      tipoOperacaoFoneticaPhonos = "2",
      situacoesSelecionadas = "",
      num_registro = "",
      parteAutor = "FALSE",
      parteReu = "FALSE",
      parteOutros = "FALSE",
      opcoesFoneticaPhonosParte = "2",
      quantidadeMinimaTermosPresentesParte = "1",
      opcoesFoneticaPhonosAdvogado = "2",
      quantidadeMinimaTermosPresentesAdvogado = "1",
      conectivo = "OU",
      listarProcessosOrdemDescrecente = "TRUE",
      listarProcessosOrdemDescrecenteTemp = "TRUE",
      listarProcessosAtivosSomente = "FALSE",
      listarProcessosEletronicosSomente = "FALSE"
    )

  ## Esse pedido é só para iniciar a sessão
  s <- "https://scon.stj.jus.br/SCON/index.jsp?novaPesquisa" %>%
    httr::GET()

  ## Essa url servirá como referer
  url2 <- "https://ww2.stj.jus.br/processo/pesquisa/?aplicacao=processos.ea"

  purrr::walk(registro_stj, purrr::possibly(~{

    body$num_registro <- .x

    arquivo <- paste0("_registro_stj_",stringr::str_replace_all(.x,"\\D","_"), ".html")

    httr::RETRY("POST", url = url, body = body,  encode="form",
                httr::add_headers(`Referer` = url2),
                httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo))))

  },NULL))
}
