#' Lê pdfs baixados com stj_baixar_docs_pdf
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar arquivos
#'
#' @return Tibble com os julgados
#' @export
#'
stj_ler_docs_pdf <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "pdf$")

  }


  pb <- progress::progress_bar$new(total = length(arquivos))


  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    sequencial <- stringr::str_extract(.x, "(?<=sequencial_)\\d+")
    registro <- stringr::str_extract(.x,"(?<=registro_)\\d+")
    data <- stringr::str_extract(.x,"(?<=data_)\\d+")
    suppressMessages(
      julgado <- pdftools::pdf_text(.x) |>
        stringr::str_c(collapse = "\n")
    )
    tibble::tibble(sequencial = sequencial, registro, data, julgado)

  },NULL))


}
