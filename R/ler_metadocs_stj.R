#' Ler metadados do STJ
#'
#' @param arquivos vetor com caminhos para arquivos
#' @param diretorio se arquivos for NULL, default para
#'     diretório atual
#' @param plano ver `future::plan`
#'
#' @return tibble com números dos processos, metadado dos docs
#'     e sequencial para ser usado na função baixar_pdf_stj
#' @export
#'
#' @examples
#' \dontrun{
#' ler_metadocs_stj(diretorio=".")
#' }
ler_metadocs_stj <- function(arquivos = NULL, diretorio = ".", plano = "sequential"){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, "_\\d{2}_processo",full.names = TRUE)

  }



  future::plan(plano)

  furrr::future_map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"(?<=stj_)\\d.+(?=\\.html)") %>%
      stringr::str_remove_all("\\D+")

    x <- xml2::read_html(.x)

    nome<-xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_text("onclick")

    sequencial <- xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("(?<=sequencial=)\\d+")

    tibble::tibble(processo,nome, sequencial) %>%
      dplyr::distinct()

  },NULL))
}
