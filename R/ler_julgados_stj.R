#' Lê julgados do STJ
#'
#' @param arquivos lista de arquivos
#' @param diretorio se arquivos não forem informados,
#'     informar diretório
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_julgados_stj()
#' }
ler_julgados_stj <- function(arquivos = NULL,
                             diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, "html", full.names = TRUE)
  }
  purrr::map_dfr(arquivos, purrr::possibly(~{

    principal <- xml2::read_html(.x)


    processo <- principal %>%
      xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][1]") %>%
      xml2::xml_text() %>%
      stringr::str_trim()

    origem <- processo %>%
      stringr::str_extract("\\w{2}$")

    classe <- principal %>%
      xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][2]") %>%
      xml2::xml_text() %>%
      stringr::str_trim()

    registro <- principal %>%
      xml2::xml_find_all("//div[@class='docTexto']/text()[preceding-sibling::br][2]") %>%
      xml2::xml_text() %>%
      stringr::str_trim()

    relator <- principal %>%
      xml2::xml_find_all("//div/h4[text()='Relator(a)']/following-sibling::pre[@class='docTexto']") %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=Ministr[ao]\\s).*(?=\\s\\()")

    orgao_julgador <- principal %>%
      xml2::xml_find_all(
        "//div/h4[text()='\u00D3rg\u00E3o Julgador']/following-sibling::pre[@class='docTexto']"
      ) %>%
      xml2::xml_text()

    data_julgamento <- principal %>%
      xml2::xml_find_all(
        "//div/h4[text()='Data do Julgamento']/following-sibling::pre[@class='docTexto']"
      ) %>%
      xml2::xml_text() %>%
      lubridate::dmy()

    publicacao <- principal %>%
      xml2::xml_find_all(
        "//div/h4[text()='Data da Publica\u00E7\u00E3o/Fonte']/following-sibling::pre[@class='docTexto']"
      ) %>%
      xml2::xml_text()

  #  fonte <- publicacao %>% stringr::str_extract("\\w+")

    data_publicacao <- pt_time_extract(publicacao)

    ementa <- principal %>%
      xml2::xml_find_all("//div/h4[text()='Ementa']/following-sibling::pre[@class='docTexto']") %>%
      xml2::xml_text()

    dispositivo <- principal %>%
      xml2::xml_find_all(
        "//div/h4[text()='Ac\u00F3rd\u00E3o']/following-sibling::pre[@class='docTexto']"
      ) %>%
      xml2::xml_text()

    tibble::tibble(
      processo,
      origem,
      classe,
      registro,
      relator,
      orgao_julgador,
      data_julgamento,
      data_publicacao,
      ementa,
      dispositivo
    )
  }, NULL))
}
