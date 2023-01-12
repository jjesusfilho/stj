#' Extrair metadados da jurisprudência do STJ
#'
#' @param livre Se deixar em branco, informe as datas.
#' @param aspas Colocar entre aspas a busca
#' @param dt_ini Data inicial do julgamento no formato dd/mm/aaaa
#' @param dt_fim Data final do julgamento no formato dd/mm/aaaa
#' @param dt_ini_pb Data inicial da publicação no formato "dd/mm/aaaa"
#' @param dt_fim_pb Data final da publicação no formato "dd/mm/aaaa"
#' @param base "ACOR" ou "MONO"
#' @param n Número de páginas a serem baixadas. Se NULL,
#'     baixa todas as páginas disponíveis
#' @param diretorio Informar onde baixar os htmls
#'
#' @return tibble com metadados
#' @export
#'
#' @examples
#' \dontrun{
#'     stj_baixar_cjsg(
#'    dt_ini = "01/07/2019",
#'     dt_fim = "30/07/2019"
#'     )
#' }
stj_baixar_cjsg  <- function(livre = "",
                                  aspas=FALSE,
                                  dt_ini = "",
                                  dt_fim = "",
                                  dt_ini_pb = "",
                                  dt_fim_pb = "",
                                  base = "ACOR",
                                  n = NULL,
                                  diretorio = "."){

  ## Variáveis

  if (base == "MONO"){

    b <- "DTXT"
    b1 <- "decisoes"
  } else {


    b <- base
    b1 <- "jurisprudencia"
    }

  base <- tolower(base)

  livre <-  abjutils::rm_accent(livre) |>
    toupper()

  if (aspas == TRUE) {
    livre <- deparse(livre)
  }


  ###
  if (dt_ini == "" & dt_fim == ""){

    dtde <- ""

  } else {




  dti <- lubridate::dmy(dt_ini) |>
    stringr::str_remove_all("\\D+")

  dtf <- lubridate::dmy(dt_fim) |>
    stringr::str_remove_all("\\D+")

  dtde <- glue::glue("@DTDE >= {dti} E @DTDE <= {dtf}")
}



  if (dt_ini_pb == "" & dt_fim_pb == ""){

    dtpb <- ""

  } else {


    dtipb <- lubridate::dmy(dt_ini_pb) |>
      stringr::str_remove_all("\\D+")

    dtfpb <- lubridate::dmy(dt_fim_pb) |>
      stringr::str_remove_all("\\D+")

    dtpb <- glue::glue("@DTPB >= {dtipb} E @DTPB <= {dtfpb}")
  }


 ## Criar a variável data a partir das demais

  if (dtde != "" & dtpb != ""){

    data <- glue::glue("({dtde}) e ({dtpb})")


  } else if (dtde != "" & dtpb == ""){

    data <- dtde


  } else {

    data <- dtpb

  }




  ## Cria di e df para colocar no nome do arquivo.
  ## Basicamente o que faz é pegar uma das datas.


  di <- c(dt_ini, dt_ini_pb) |>
        stringr::str_subset("^$", negate = T) |>
        utils::head(1) |>
        stringr::str_replace_all("\\D","_")

  df <- c(dt_fim, dt_fim_pb) |>
    stringr::str_subset("^$", negate = T) |>
    utils::head(1) |>
    stringr::str_replace_all("\\D","_")

  #open_search <- utils::URLencode(open_search)

  httr::set_config(httr::config(ssl_verifypeer = 0L))

  url1 <- "http://www.stj.jus.br/SCON/"
  r1 <- httr::GET(url1)

  body <-
    list(
      pesquisaAmigavel = "",
      acao = "pesquisar",
      novaConsulta = "true",
      i = "1",
      b = b,
      livre = livre,
      filtroPorOrgao = "",
      filtroPorMinistro = "",
      filtroPorNota = "",
      data = data,
      operador = "e",
      thesaurus = "JURIDICO",
      p = "true",
      tp = "T",
      processo = "",
      classe = "",
      uf = "",
      relator = "",
      dtpb = dtpb,
      dtpb1 = dt_ini_pb,
      dtpb2 = dt_fim_pb,
      dtde = dtde,
      dtde1 = dt_ini,
      dtde2 = dt_fim,
      orgao = "",
      ementa = "",
      nota = "",
      ref = ""
    )

  aceita <- "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"
  encoding <- "gzip, deflate, br"
  conteudo_tipo ="application/x-www-form-urlencoded"

  cook <- unlist(r1$cookies)

  referer <- "https://scon.stj.jus.br/SCON/"

  agente <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"



  url2 <- "https://scon.stj.jus.br/SCON/pesquisar.jsp"

  r2 <- httr::POST(url2, body = body,
             encode = "form",
             httr::set_cookies(cook),
             httr::user_agent(agente),
             httr::accept(aceita),
             httr::content_type(conteudo_tipo),
             httr::add_headers(Connection = "keep-alive",
                         `sec-ch-ua`= '"Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"',
                         `sec-ch-ua-mobile` = "?0",
                         `sec-ch-ua-platform` = "Windows",
                         `Sec-Fetch-Dest` =  "document",
                         `Sec-Fetch-Mode` =  "navigate",
                         `Sec-Fetch-Site` =  "same-origin",
                         `Sec-Fetch-User` =  "?1",
                         `Upgrade-Insecure-Requests` =  1,
                         `Accept-Encoding` = "gzip, deflate, br",
                         `Accept-Language` =  "pt-PT,pt;q=0.9,en-US;q=0.8,en;q=0.7",
                         `Cache-Control` =  "no-cache",
                         Referer = referer
             ))




  conteudo <- httr::content(r2)


   if (is.null(n)){

  pages <- conteudo |>
    xml2::xml_find_first("//span[@class='numDocs']") |>
    xml2::xml_text() |>
    stringr::str_extract("\\d+\\.?\\d*") |>
    stringr::str_remove_all("\\D") |>
    as.numeric()
  } else {

    pages <- n*10
  }



  ## Might be important to inform the exact request time in Brasilia
  ## in order to allow others to replicate the research.

  search_time <-lubridate::now(tz="America/Sao_Paulo")

  seq(1,pages,10) |>
    purrr::walk(purrr::possibly(~{

      arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_",base,"_data_inicial_",di, "_data_final_",df,"_pagina_",.x, ".html"))

      httr::POST(paste0("https://scon.stj.jus.br/SCON/",b1,"/toc.jsp"),
                body = list(
                  numDocsPagina = "10",
                  tipo_visualizacao = "",
                  filtroPorOrgao = "",
                  filtroPorMinistro = "",
                  filtroPorNota = "",
                  b = b,
                  p = "true",
                  data = data,
                  operador = "e",
                  l = 10,
                  livre = livre,
                  i = .x,
                  tp = TRUE
                ), encode = "form",
                httr::set_cookies(unlist(r2$cookies)),
                httr::user_agent(agente),
                httr::accept(aceita),
                httr::content_type(conteudo_tipo),
                httr::add_headers(Connection = "keep-alive",
                                  `sec-ch-ua`= '"Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"',
                                  `sec-ch-ua-mobile` = "?0",
                                  `sec-ch-ua-platform` = "Windows",
                                  `Sec-Fetch-Dest` =  "document",
                                  `Sec-Fetch-Mode` =  "navigate",
                                  `Sec-Fetch-Site` =  "same-origin",
                                  `Sec-Fetch-User` =  "?1",
                                  `Upgrade-Insecure-Requests` =  1,
                                  `Accept-Encoding` = "gzip, deflate, br",
                                  `Accept-Language` =  "pt-PT,pt;q=0.9,en-US;q=0.8,en;q=0.7",
                                  `Cache-Control` =  "no-cache",
                                  Referer = url2
                ),

                        httr::write_disk(arquivo,overwrite = T))


    },NULL))

}
