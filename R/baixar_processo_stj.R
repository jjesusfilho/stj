#' Baixa processos do stj pelo número do do processo ou do registro
#'
#' @param numero Número do processo ou do registro
#' @param diretorio Diretório. Default para atual
#' @description Você pode informar tanto o número do processo,
#'     quanto o número do registro.
#'
#' @return html baixado no diretório indicado
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_processo_stj("2016/0074331-0")
#' baixar_processo_stj("01673163020133000000")
#' baixar_processo_stj("0167316-30.2013.3.00.0000")
#'
#' }
baixar_processo_stj <- function(numero = NULL, diretorio = ".") {

  if (stringr::str_remove_all(numero[1],"\\D+") %>% nchar() == 20){

    numero <- purrr::map(numero,~stringr::str_remove_all(.x,"\\D+")) %>%
          unlist() %>%
            abjutils::build_id()

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
        origemOrgaosSelecionados = "",
        origemUFSelecionados = "",
        julgadorOrgaoSelecionados = "",
        tipoRamosDireitoSelecionados = "",
        situacoesSelecionadas = "",
        num_processo = "",
        num_registro = "",
        numeroUnico = "",
        numeroOriginario = "",
        advogadoCodigo = "",
        dataAutuacaoInicial = "",
        dataAutuacaoFinal = "",
        pautaPublicacaoDataInicial = "",
        pautaPublicacaoDataFinal = "",
        dataPublicacaoInicial = "",
        dataPublicacaoFinal = "",
        parteAutor = "FALSE",
        parteReu = "FALSE",
        parteOutros = "FALSE",
        parteNome = "",
        opcoesFoneticaPhonosParte = "2",
        quantidadeMinimaTermosPresentesParte = "1",
        advogadoNome = "",
        opcoesFoneticaPhonosAdvogado = "2",
        quantidadeMinimaTermosPresentesAdvogado = "1",
        conectivo = "OU",
        listarProcessosOrdemDescrecente = "TRUE",
        listarProcessosOrdemDescrecenteTemp = "TRUE",
        listarProcessosAtivosSomente = "FALSE",
        listarProcessosEletronicosSomente = "FALSE"
      )



  } else {

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


      }


  url <- "https://ww2.stj.jus.br/processo/pesquisa/"


  ## Esse pedido é só para iniciar a sessão
  s <- "https://scon.stj.jus.br/SCON/index.jsp?novaPesquisa" %>%
    httr::GET()

  ## Essa url servirá como referer
  url2 <- "https://ww2.stj.jus.br/processo/pesquisa/?aplicacao=processos.ea"

  purrr::walk(numero, purrr::possibly(~{

    if (stringr::str_remove_all(.x,"\\D+") %>% nchar() == 20){
    body$numeroUnico <- .x
    } else {
     body$num_registro <- .x

   }
    arquivo <- paste0("_registro_stj_",stringr::str_replace_all(.x,"\\D","_"), ".html")

    httr::RETRY("POST", url = url, body = body,  encode="form",
                httr::add_headers(`Referer` = url2),
                httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo))))

  },NULL))
}
