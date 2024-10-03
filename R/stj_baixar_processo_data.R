#' Baixa processos por data
#'
#' @param dt_inicio Data inicial no formato dd/mm/aaaa. Se não informar,
#'     baixa os processos de hoje.
#' @param dt_fim Data final no formato dd/mm/aaaa.
#'     Se não informar,  baixa os processos de hoje.
#' @param diretorio Onde armazenar os htmls
#'
#' @return html
#' @export
#'
stj_baixar_processo_data <- function(dt_inicio = NULL, dt_fim = NULL, diretorio = "."){

  if(is.null(dt_inicio)){

   dt_inicio <-  Sys.Date() |> format("%d/%m/%Y")

  }



  if(is.null(dt_fim)){

    dt_fim <- Sys.Date() |> format("%d/%m/%Y")

  }

  dt_inicio <- lubridate::dmy(dt_inicio) |> format("%d/%m/%Y")

  dt_fim <- lubridate::dmy(dt_fim) |> format("%d/%m/%Y")

  ver <- c(dt_fim, dt_inicio) |>
         is.na() |>
         any()

  if(ver){

    stop("dt_inicio e dt_fim devem estar no formato dd/mm/yyyy")

  }





url1 <- "https://processo.stj.jus.br/processo/pesquisa/"

url2 <- "https://processo.stj.jus.br/processo/pesquisa/?aplicacao=processos.ea"


body <-
  list(
    aplicacao = "processos.ea",
    acao = "pushconsultarprocessoconsultalimitenaoatendidasjaincluidas",
    descemail = "",
    senha = "",
    totalRegistrosPorPagina = "100",
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
    dataAutuacaoInicial = dt_inicio,
    dataAutuacaoFinal = dt_fim,
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

r1 <- httr::POST(url1, httr::add_headers(Referer = url2),
                 body  = body, encode = "form")

di <- stringr::str_replace_all(dt_inicio, "/","_")

df <- stringr::str_replace_all(dt_fim, "/","_")


arquivo <- file.path(diretorio, paste0("dt_inicio_",di, "_dt_fim_", df,"_pagina_1", ".html"))

writeBin(r1$content, arquivo)

registros <- r1 |>
      httr::content() |>
      xml2::xml_find_first("//span[@class='classSpanPaginacaoPaginaTextoInterno']") |>
      xml2::xml_find_first("//div[@class='clsMensagemLinha']/b") |>
      xml2::xml_text() |>
      as.integer()

paginas <- (registros/100) |>
            floor()

body$`NumTotalRegistros` <- registros
body$`VaiParaPaginaAnterior` <- FALSE
body$`VaiParaPaginaSeguinte` <- TRUE

purrr::walk(1:paginas, purrr::possibly(~{

arquivo <- file.path(diretorio, paste0("dt_inicio_",di, "_dt_fim_", df,"_pagina_",.x+1, ".html"))


body$`NumPaginaAtual` <- .x


httr::POST(url1, httr::add_headers(Referer = url2),
           body  = body, encode = "form", httr::write_disk(arquivo, overwrite =TRUE))

}, NULL), .progress = TRUE)

}
