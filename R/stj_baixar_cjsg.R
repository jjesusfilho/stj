#' Extrair metadados da jurisprudência do STJ
#'
#' @param livre Se deixar em branco, informe as datas.
#' @param aspas Colocar entre aspas a busca
#' @param data_inicial Data inicial da publicação no formato "dd/mm/aaaa"
#' @param data_final Data final da publicação no formato "dd/mm/aaaa"
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
#'    data_inicial = "01/07/2019",
#'     data_final = "30/07/2019"
#'     )
#' }
stj_baixar_cjsg  <- function(livre = "",
                                  aspas=FALSE,
                                  data_inicial = "",
                                  data_final = "",
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

  livre <-  abjutils::rm_accent(livre) %>%
    toupper()

  if (aspas == TRUE) {
    livre <- deparse(livre)
  }

  dt <- lubridate::dmy(data_inicial) %>%
    stringr::str_remove_all("\\D+")

  df <- lubridate::dmy(data_final) %>%
    stringr::str_remove_all("\\D+")

  data <- glue::glue("@DTPB >= {dt} E @DTPB <= {df}")



  #open_search <- utils::URLencode(open_search)

  url1 <- "http://www.stj.jus.br/SCON/"
  url2 <- "http://www.stj.jus.br/SCON/pesquisar.jsp"
  h <- httr::GET(url1)

  ####corpo_fim####


  content<-httr::GET("https://scon.stj.jus.br",
                     path = "SCON/jurisprudencia/pesquisaAjax.jsp",
                     query =
                       list(pagina = paste0("campo",b),
                            p = "true",
                            data = data,
                            livre = livre,
                            b = b)) %>%
    httr::content()


  ## Get the number of pages to loop over each one and add them to the data.frame

  if (is.null(n)){

  pages <- content %>%
    #xml2::xml_find_first(xpath = '//div[@id="infopesquisa"]/span[@class="labellinha"]/following-sibling::span[1]') %>%
    xml2::xml_text() %>%
    stringr::str_squish() %>%
    stringr::str_remove_all("\\D+") %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()
  } else {

    pages <- n*10
  }



  ## Might be important to inform the exact request time in Brasilia
  ## in order to allow others to replicate the research.

  search_time<-lubridate::now(tz="America/Sao_Paulo")

  seq(1,pages,10) %>%
    purrr::walk(purrr::possibly(~{

      arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_",base,"_data_inicial_",dt, "_data_final_",df,"_pagina_",.x, ".html"))

      httr::POST(paste0("https://scon.stj.jus.br/SCON/",b1,"/toc.jsp"),
                body = list(
                  numDocsPagina = "10",
                  tipo_visualizacao = "",
                  filtroPorOrgao = "",
                  filtroPorMinistro = "",
                  b = b,
                  p = "true",
                  data = data,
                  operador = "e",
                  l = 10,
                  livre = livre,
                  i = .x,
                  b = "ACOR"
                ), encode = "form",
                    #    httr::add_headers(`Referer` = url2),
                        httr::write_disk(arquivo,overwrite = T))


    },NULL))

}
