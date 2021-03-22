#' Lê decisões monocráticas do STJ
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informar arquivos, informar diretório
#'
#' @return Tibble
#' @export
#'
stj_ler_cjsg_mono <- function(arquivos = NULL, diretorio =  "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern = "mono", full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

purrr::map_dfr(arquivos, purrr::possibly(~{

 pb$tick()

 x <- xml2::read_html(.x)

hora_baixa <- stringr::str_extract(.x,"\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}") %>%
              lubridate::ymd_hms()

pagina <- stringr::str_extract(.x,"\\d+(?=\\.html)")

processo <- x %>%
  xml2::xml_find_all("//div[text()='Processo']/following-sibling::div") %>%
  xml2::xml_text(trim = TRUE)

relator <- x %>%
    xml2::xml_find_all("//div[text()='Relator(a)']/following-sibling::div") %>%
    xml2::xml_text(trim = TRUE) %>%
    stringr::str_remove("(?i)ministr[oa] ")




data_publicacao <- x %>%
  xml2::xml_find_all("//div[text()='Data da Publica\u00e7\u00e3o']/following-sibling::div") %>%
  xml2::xml_text(trim = TRUE) %>%
  lubridate::dmy()


registro <- x %>%
       xml2::xml_find_all("//div/a[@title='Consulta Processual']") %>%
       xml2::xml_attr("href") %>%
       stringr::str_extract("\\d+") %>%
       stringr::str_replace("(\\d{4})(\\d+)(\\d$)","\\1/\\2-\\3")

decisao  <- x %>%
  xml2::xml_find_all("//div[text()='Decis\u00e3o']/following-sibling::div/p") %>%
  purrr::map_chr(~{
    .x %>%
      xml2::xml_contents() %>%
      xml2::xml_text(trim = TRUE) %>%
      stringr::str_c(collapse = "\n")
})

titulo <- decisao %>%
       stringr::str_extract(".+")

classe <- titulo %>%
          stringr::str_extract("(?i).+(?= N\u00ba)")

numero <- titulo %>%
          stringr::str_extract("(?i)(?<=n\u00ba )\\S+") %>%
          stringr::str_remove_all("\\D")

origem <- titulo %>%
          stringr::str_extract("(?<=- )\\w+(?= \\()")

registro <- titulo %>%
           stringr::str_extract("(?<=\\()\\d.+(?=\\))")

tibble::tibble(pagina, hora_baixa, processo, classe, numero, origem,registro, relator,data_publicacao, decisao)

},NULL))

}
