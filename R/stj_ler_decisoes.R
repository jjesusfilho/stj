#' Ler decisões baixadas com stj_baixar_decisoes
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório se não informar arquivos
#' @param assinatura Manter assinatura no pé da página?
#'
#' @return Tibble
#' @export
#'
stj_ler_decisoes <- function(arquivos = NULL,
                             diretorio = ".",
                             assinatura = FALSE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "pdf$")
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    registro <- stringr::str_extract(.x, "(?<=registro_)\\d+")
    sequencial <- stringr::str_extract(.x, "(?<=sequencial_)\\d+")

    texto <- pdftools::pdf_text(.x)

    if (assinatura == FALSE){

      texto <-  texto |>
        stringr::str_remove_all("\nEdi\u00E7\u00E3o\\sn\u00BA\\s\\d\\X+Controle do Documento:\\s\\S+")

    }

    texto <- texto |>
      stringr::str_c(collapse = "\n\n")

    tibble::tibble(registro, sequencial, julgado = texto)


  },NULL))


}
