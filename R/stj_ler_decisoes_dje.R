#' Ler decisões do STJ baixados do dje
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente, informar diretórios.
#' @param assinatura FALSE para remover as assinaturas.
#' @return tibble
#' @export
#'
stj_ler_decisoes_dje <- function(arquivos  = NULL,
                                 diretorio = ".",
                                 assinatura = FALSE){


  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "pdf$")
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()


    registro <- .x |>
      stringr::str_extract("(?<=registro_)\\d+")

    data <- .x |>
      stringr::str_extract("(?<=data_)\\d{2}_\\d{2}_\\d{4}") |>
      lubridate::dmy()

    seq_documento <- .x |>
      stringr::str_extract("(?<=seq_documento_)\\d+")

    nu_seguimento <- .x |>
      stringr::str_extract("(?<=nu_seguimento_)\\d+")

    texto <- pdftools::pdf_text(.x)

    if (assinatura == FALSE){

      texto <-  texto |>
        stringr::str_remove_all("\nEdi\u00E7\u00E3o\\sn\u00BA\\s\\d\\X+Controle do Documento:\\s\\S+")

    }

    texto <- texto |>
      stringr::str_c(collapse = "\n\n")


    tibble::tibble(registro,
                   data,
                   seq_documento,
                   nu_seguimento,
                   julgado = texto)
  }, NULL))
}
