#' Baixar decisões do STJ
#'
#' @param livre campo livre
#' @param operador "e" ou "adj", padrão para "e"
#' @param aspas colocar entre aspas? padrão para TRUE
#' @param repo informar o repositóriorio, padrão para "ACOR"
#' @param data_inicial data no formato "dd/mm/yyyy"
#' @param data_final  data no formato "dd/mm/yyyy"
#' @param diretorio padrão para diretorio atual
#'
#' @return htmls
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_julgados_stj(livre= "dissolu\u00e7\u00e3o irregular")
#' }
baixar_julgados_stj<- function(livre = "", operador = "e", aspas = FALSE, repo = c("ACOR","SUMU","DTXT","INFJ"), data_inicial= "", data_final = "",diretorio = "." ){

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  livre<-  abjutils::rm_accent(livre)

  if (aspas == TRUE) {
    livre <- deparse(livre)
  }

  if (operador != "e" & operador != "adj"){
    stop("Informar se o operador \u00e9 'e' ou 'adj'")
  }

  repo <- repo %>%
    `[`(1) %>%
    toupper()

  url1 <- "http://www.stj.jus.br/SCON/"
  url2 <- "http://www.stj.jus.br/SCON/pesquisar.jsp"


  h <- httr::GET(url1)


  ####corpo_fim####

  inicial<- data_inicial %>%
         lubridate::dmy() %>%
         format("%Y%m%d")

  final<- data_final %>%
    lubridate::dmy() %>%
    format("%Y%m%d")

 data<- paste0("@DTDE >= ",inicial," e @DTDE <= ",final)

  content<-httr::GET("http://www.stj.jus.br",
                     path = "/SCON/jurisprudencia/pesquisaAjax.jsp",
                     query = list(tipo_visualizacao = "",
                                  novaConsulta = TRUE,
                                  acao ="pesquisar",
                                  data = data,
                                  livre = livre,
                                  b = repo,
                                  operador = operador,
                                  opAjuda = "SIM",
                                  p = FALSE,
                                 # l = 10,
                                #  t = 'JURIDICO',
                                  tipo_data= "DTDE",
                                  data_inicial =data_inicial,
                                  data_final =data_final,
                                  i = 1),
                     httr::add_headers(`Referer` = url2))%>%
    httr::content()


  ## Get the number of pages to loop over each one and add them to the data.frame
  # pages <- content %>%
  #   xml2::xml_find_first(xpath = '//div[@id="infopesquisa"]/span[@class="labellinha"]/following-sibling::span[1]') %>%
  #   xml2::xml_text() %>%
  #   as.numeric()

  paginas<- content %>%
    xml2::xml_find_first("//span[@class='labellinha']/following-sibling::span") %>%
    xml2::xml_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()


  ## Might be important to inform the exact request time in Brasilia
  ## in order to allow others to replicate the research.



seq(1,paginas,10) %>%
  purrr::walk(purrr::possibly(~{

    arquivo<-paste0("_pagina_",.x,".html")
    httr::GET("http://www.stj.jus.br",
                      path = "/SCON/jurisprudencia/toc.jsp",
                      query = list(tipo_visualizacao = "",
                                   data= data,
                                   livre = livre,
                                   b = repo,
                                   operador = operador,
                                   p = "true",
                                   l = 10,
                                   t = 'JURIDICO',
                                   tipo_data= "DTDE",
                                   data_inicial =data_inicial,
                                   data_final =data_final,
                                   i = .x),
                      httr::add_headers(`Referer` = url2),
                      httr::write_disk(file.path(diretorio, Sys.time() %>%
                                                   stringr::str_replace_all("\\D+", "_") %>%
                                                   stringr::str_replace("$", arquivo))))

  },NULL))

  }
