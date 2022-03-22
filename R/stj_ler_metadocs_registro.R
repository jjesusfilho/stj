#' Ler metadados do STJ
#'
#' @param arquivos vetor com caminhos para arquivos
#' @param diretorio se arquivos for NULL, default para
#'     diretório atual
#'
#' @return tibble com números dos processos, metadado dos docs
#'     e sequencial para ser usado na função baixar_pdf_stj
#' @export
#'
#' @examples
#' \dontrun{
#' stj_ler_metadocs(diretorio=".")
#' }
stj_ler_metadocs_registro <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, "_\\d{2}_(processo|registro)",full.names = TRUE)

  }




  purrr::map_dfr(arquivos,purrr::possibly(~{

    registro <- stringr::str_extract(.x,"(?<=stj_)\\d.+(?=\\.html)") %>%
      stringr::str_remove_all("\\D+")

    x <- xml2::read_html(.x)

    nome<-xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_text("onclick")

    sequencial <- xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("(?<=sequencial=)\\d+")

    peticao_numero <-
      xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("(?<=peticao_numero=)\\d+")

     data_publicacao <- nome |>
          stringr::str_extract("\\d{2}/\\d{2}/\\d+") |>
          lubridate::dmy()

     classe_numero <- nome |>
         stringr::str_extract("\\w+\\s+\\d+(?=\\()") |>
         stringr::str_squish()

     classe <- classe_numero |>
               stringr::str_extract("^\\D+")

     numero <- classe_numero |>
           stringr::str_extract("\\d+")

    tibble::tibble(registro,classe, numero,nome, sequencial,peticao_numero, data_publicacao) %>%
      dplyr::distinct() |>
      tidyr::fill(data_publicacao, .direction = "down")

  },NULL))
}
