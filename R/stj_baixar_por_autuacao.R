#' Baixa consulta de processos do STJ por data da autuacao
#'
#' @param data_inicial Data inicial no formato dd/mm/aaaa.
#' @param data_final Data final no formato dd/mm/aaaa.
#' @param origem Sigla do(s) estado(s) de origem.
#' @param ramo Sigla do ramo do direito. Verifique o ramo
#'      por meio do dataset incluso ramo.
#' @param n Número de páginas.
#' @param diretorio Diretório. Se não informado, atual.
#'
#' @return Arquivos em csv
#' @export
#'
#' @examples
#' \dontrun{
#' stj_baixar_por_autuacao(data_inicial = "01/01/2019",
#'                           data_final = "30/01/2019",
#'                           origem = c("SP", "AC"))
#' }
#'
stj_baixar_por_autuacao <- function(data_inicial,
                                      data_final,
                                      origem = "",
                                      ramo = "",
                                      n = NULL,
                                      diretorio = "."){


  origem <- origem |>
    toupper() |>
    stringr::str_c(collapse= ",")


  ramo <- ramo |>
    toupper() |>
    stringr::str_c(collapse= ",")

  ## Essa url servirá como referer
  url2 <- "https://processo.stj.jus.br/processo/pesquisa/?aplicacao=processos"

  url <- "https://processo.stj.jus.br/processo/pesquisa/"


  httr::set_config(httr::config(ssl_verifypeer = 0L))

    corpo <-
      list(
        aplicacao = "processos",
        acao = "pushconsultarprocessoconsultalimitenaoatendidasjaincluidas",
        descemail = "",
        senha = "",
        totalRegistrosPorPagina = "1",
        tipoPesquisaSecundaria = "",
        sequenciaisParteAdvogado = "-1",
        refinamentoAdvogado = "",
        refinamentoParte = "",
        tipoOperacaoFonetica = "",
        tipoOperacaoFoneticaPhonos = "2",
        origemOrgaosSelecionados = "",
        origemUFSelecionados = origem,
        julgadorOrgaoSelecionados = "",
        tipoRamosDireitoSelecionados = ramo,
        situacoesSelecionadas = "",
        num_processo = "",
        num_registro = "",
        numeroUnico = "",
        numeroOriginario = "",
        advogadoCodigo = "",
        dataAutuacaoInicial = data_inicial,
        dataAutuacaoFinal =data_final,
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


    resposta<-  httr::RETRY("POST", url = url, body = corpo,  encode="form",
                            httr::add_headers(`Referer` = url2),
                            httr::timeout(30))


    total  <- resposta |>
      httr::content()|>
      xml2::xml_find_first("//div[@class='clsMensagemLinha']") |>
      xml2::xml_text(trim = TRUE) |>
      stringr::str_extract("\\d+") |>
      as.integer()

    dividir <- \(x) x/40

    paginas <- total |>
      dividir() |>
      ceiling()

  if (!is.null(n)){

  paginas <- n

  }

  corpo <-  list(
    aplicacao = "processos",
    acao = "pushconsultarprocessoconsultalimitenaoatendidasjaincluidas",
    descemail = "",
    senha = "",
    NumPaginaAtual = "1",
    NumTotalRegistros = total,
    VaiParaPaginaAnterior = "false",
    VaiParaPaginaSeguinte = "true",
    ComProximaPagina = "TRUE",
    totalRegistrosPorPagina = 40,
    tipoPesquisaSecundaria = "",
    sequenciaisParteAdvogado = "-1",
    refinamentoAdvogado = "",
    refinamentoParte = "",
    tipoOperacaoFonetica = "",
    tipoOperacaoFoneticaPhonos = "2",
    origemOrgaosSelecionados = "",
    origemUFSelecionados = origem,
    julgadorOrgaoSelecionados = "",
    tipoRamosDireitoSelecionados = ramo,
    situacoesSelecionadas = "",
    num_processo = "",
    num_registro = "",
    numeroUnico = "",
    numeroOriginario = "",
    advogadoCodigo = "",
    dataAutuacaoInicial = data_inicial,
    dataAutuacaoFinal = data_final,
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

  pb <- progress::progress_bar$new(total = paginas)

  di <- data_inicial |>
    stringr::str_replace_all("/","_")

  df <- data_final |>
    stringr::str_replace_all("/","_")




  purrr::walk(0:paginas,purrr::possibly(~{

    pb$tick()



    arquivo <- file.path(diretorio, paste0("stj_por_autuacao_data_inicial_",di,"_data_final_",df,"_pagina_",.x, ".csv"))



    corpo$NumPaginaAtual <- .x


    httr::RETRY("POST", url = url, body = corpo,  encode="form",
                httr::add_headers(`Referer` = url2),
                httr::timeout(30)) |>
    httr::content() |>
      xml2::xml_find_first("//textarea[@class='clsCopiarParaCSV']") |>
      xml2::xml_text() |>
      cat(file = arquivo)




  }, NULL))
}
