#' Baixa processos do stj pelo número originário do processo do processo ou do registro
#'
#' @param numero Número do processo originário, ou seja, do TJ de origem
#' @param diretorio Diretório. Default para atual
#' @param documentos Se TRUE, baixa também documentos
#' @description Você pode informar tanto o número do processo,
#'     quanto o número do registro.
#'
#' @return html baixado no diretório indicado
#' @export
#'

stj_baixar_processo_originario <- function(numero = NULL, diretorio = ".",documentos = FALSE) {

  httr::set_config(httr::config(ssl_verifypeer = 0L))


  numero <- stringr::str_remove_all(numero, "\\D+")

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




  url <- "https://processo.stj.jus.br/processo/pesquisa/"


  ## Esse pedido é só para iniciar a sessão
  # s <- "https://scon.stj.jus.br/SCON/index.jsp?novaPesquisa" %>%
  #   httr::GET()

  ## Essa url servirá como referer
  url2 <- "https://processo.stj.jus.br/processo/pesquisa/?aplicacao=processos.ea"


  purrr::walk(numero, purrr::possibly(~{

    body$numeroOriginario <- .x


    arquivo <- paste0("_originario_",stringr::str_replace_all(.x,"\\D","_"), ".html")

    resposta<-  httr::RETRY("POST", url = url, body = body,  encode="form",
                            httr::add_headers(`Referer` = url2),
                            httr::timeout(30))

    ## Segue adiante apenas se o html não estiver vazio,
    ## ou seja, se o processo realmente existir:
    if(resposta$content %>% object.size() > 200000){

      resposta$content %>%
        writeBin(file.path(diretorio, Sys.time() %>%
                             stringr::str_replace_all("\\D+", "_") %>%
                             stringr::str_replace("$", arquivo)))

      if (documentos == TRUE) {

        sequencial<-  resposta %>%
          httr::content() %>%
          xml2::xml_find_all("//*[contains(@onclick,'sequencial')]") %>%
          xml2::xml_attr("onclick") %>%
          stringr::str_extract("(?<=sequencial=)\\d+") %>%
          unique()

        url <- paste0("https://processo.stj.jus.br/websecstj/cgi/revista/REJ.cgi/ATC?seq=",sequencial,"&tipo=0&nreg=&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=&formato=HTML&salvar=false")

        purrr::walk2(url,sequencial,purrr::possibly(~{

          arquivo <- paste0("_documento_",.y,tipo,numero,".html") %>%
            stringr::str_replace_all("\\W(?!html)","_")

          httr::RETRY("GET", url = .x, httr::timeout(30),
                      httr::write_disk(file.path(diretorio, Sys.time() %>%
                                                   stringr::str_replace_all("\\D", "_") %>%
                                                   stringr::str_replace("$", arquivo))))

        },NULL))

      }
    }

  },NULL))

}
