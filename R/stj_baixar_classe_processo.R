#' Baixar processo por número do STJ: classe + numero
#'
#' @param classe Classe processsual
#' @param numero Número
#' @param diretorio Diretório onde armazenar os htmls
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' stj_baixar_classe_processo(classe = "hc", numero = "200000")
#' }
stj_baixar_classe_processo <- function(classe, numero, diretorio){

  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  url <- "https://processo.stj.jus.br/processo/pesquisa/"


  ## Essa url servirá como referer
  url2 <- "https://processo.stj.jus.br/processo/pesquisa/?aplicacao=processos.ea"


  corpo <- list(
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

  purrr::walk2(classe, numero, purrr::possibly(~{

    numero <- stringr::str_c(.x,"+",.y) |>
      stringr::str_to_lower() |>
      stringr::str_squish()

    corpo$num_processo <- numero

    arquivo <- file.path(diretorio, stringr::str_c("stj_classe_", .x, "_numero_", .y, ".html"))

    httr::RETRY("POST", url = url, body = corpo,  encode="form",
                            httr::add_headers(`Referer` = url2),
                            httr::timeout(30), httr::write_disk(arquivo, overwrite = TRUE))


  },NULL))



}

