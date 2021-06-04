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
#' ler_metadocs_stj(diretorio=".")
#' }
ler_metadocs_stj <- function(arquivos = NULL, diretorio = "."){

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

     data_publicacao <-
      xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("(?<=publicacao_data=)\\d+")

    tibble::tibble(registro,nome, sequencial,peticao_numero, data_publicacao) %>%
      dplyr::distinct()

  },NULL))
}
