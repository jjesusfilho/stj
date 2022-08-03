#' Esta função está sob manutenção
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
stj_baixar_julgados <- function(livre = "", operador = "e", aspas = FALSE, repo = c("ACOR","SUMU","DTXT","INFJ"), data_inicial= "", data_final = "",diretorio = "." ){

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  livre<-  abjutils::rm_accent(livre)


  repo <- repo |>
    purrr::pluck(1) |>
    toupper()

  url1 <- "https://scon.stj.jus.br/SCON"
  url2 <- "https://scon.stj.jus.br/SCON/pesquisar.jsp"


  h <- httr::GET(url1)


  ####corpo_fim####

  inicial<- data_inicial |>
         lubridate::dmy() |>
         format("%Y%m%d")

  if (is.na(inicial)) inicial <- ""

  final<- data_final |>
    lubridate::dmy() |>
    format("%Y%m%d")

  if (is.na(final)) final <- ""


 data<- paste0("@DTDE >= ",inicial," e @DTDE <= ",final)

 if (final == "" & inicial == "") data <- ""

 r2 <- httr::POST(url2, body = body, encode = "form",
                  httr::write_disk("data-raw/r1.html", overwrite = T))

 body <- list(
   pesquisaAmigavel = "+<b>homicidio</b>",
   acao = "pesquisar",
   novaConsulta = "true",
   i = "1",
   b = repo,
   livre = livre,
   filtroPorOrgao = "",
   filtroPorMinistro = "",
   filtroPorNota = "",
   data = data,
   operador = operador,
   thesaurus = "JURIDICO",
   p = "true",
   tp = "T",
   processo = "",
   classe = "",
   uf = "",
   relator = "",
   dtpb = "",
   dtpb1 = "",
   dtpb2 = "",
   dtde = data,
   dtde1 = data_inicial,
   dtde2 = data_final,
   orgao = "",
   ementa = "",
   nota = "",
   ref = ""
 )

 r2 <- httr::POST(url2, body = body, encode = "form",
                  httr::accept("text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"),
                  httr::content_type("application/x-www-form-urlencoded"),
                  httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:103.0) Gecko/20100101 Firefox/103.0"))




  paginas<- content |>
    xml2::xml_find_first("//span[@class='numDocs']") |>
    xml2::xml_text() |>
    stringr::str_extract("\\d+") |>
    as.numeric()


  ## Might be important to inform the exact request time in Brasilia
  ## in order to allow others to replicate the research.

sequencia <- seq(1,paginas,10)

pb <- progress::progress_bar$new(total = length(sequencia))

  purrr::walk(sequencia, purrr::possibly(~{

    pb$tick()


   arquivo <-  file.path(diretorio, Sys.time() |>
                stringr::str_replace_all("\\D+", "_") |>
                stringr::str_replace("$", paste0("_pagina_",.x,".html")))


   body <- list(
     numDocsPagina = "10",
     tipo_visualizacao = "",
     filtroPorNota = "",
     ref = "",
     data = data,
     p = "true",
     b = repo,
     pesquisaAmigavel = "",
     thesaurus = "JURIDICO",
     i = .x,
     l = "10",
     tp = "T",
     operador = "e",
     livre = livre
   )

   httr::POST(url2, body = body, encode = "form", httr::write_disk(arquivo))


  },NULL))

  }
