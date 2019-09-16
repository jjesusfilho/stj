#' Ler documentos do STJ
#'
#' @param diretorio informar diretório, apenas se
#'     não informar arquivos.
#' @param arquivos  se arquivos for NULL,
#'     informar diretório.
#' @param formato "html" ou "pdf"
#'
#' @return tibble com sequenciais e documentos
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_documento_pdf(diretorio = ".")
#' }
ler_documento_stj <- function(diretorio = ".", arquivos = NULL, formato = c("html","pdf")){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names=TRUE)
  }

  formato <- formato %>%
        `[`(1)

  purrr::map_dfr(arquivos,purrr::possibly(~{

    sequencial <- stringr::str_extract(.x,"(?<=documento_)\\d+")

    if (formato =="html"){
    documento <- xml2::read_html(.x) %>%
      xml2::xml_text() %>%
      stringr::str_replace_all("\r\n","<br>") %>%
      htmltools::HTML()
    } else {

      documento <- pdftools::pdf_text(.x) %>%
        paste0(collapse = "\n") %>%
        stringr::str_replace_all("\r\n", "<br>") %>%
        htmltools::HTML()

    }


    tibble::tibble(sequencial,documento)


  },NULL))

}
