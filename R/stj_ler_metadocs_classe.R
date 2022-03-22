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
stj_ler_metadocs_classe <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, "classe",full.names = TRUE)

  }




  purrr::map_dfr(arquivos,purrr::possibly(~{



    classe <- stringr::str_extract(.x, "(?<=classe_)[a-z]+")
    numero <- stringr::str_extract(.x, "(?<=numero_)\\d+")

    x <- xml2::read_html(.x)

    nome <- xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
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

    tibble::tibble(classe, numero,nome, sequencial,peticao_numero, data_publicacao) %>%
      dplyr::distinct()

  },NULL))
}
