#' Lê htmls baixados com stj_baixar_por_publicacao
#'
#' @param arquivos Vetor com os caminhos dos arquivos html
#' @param diretorio Diretório dos arquivos se não informar arquivos
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- stj_ler_por_publicacao
#' }
stj_ler_por_publicacao <- function(arquivos = NULL, diretorio  = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "html$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))


  purrr::map_dfr(arquivos,purrr::possibly(~{

   pb$tick()

    x <- .x |>
      xml2::read_html()


   registro <- x  |>
      xml2::xml_find_all('//span[@class="clsBlocoProcessoColuna clsBlocoProcessoColuna1 classSpanProcessoUF"]/a') %>%
      xml2::xml_attr('href')  %>%
      stringr::str_extract("(?<=termo=)\\d+")

  processo <- x  |>
      xml2::xml_find_all('//span[@class="clsBlocoProcessoColuna clsBlocoProcessoColuna1 classSpanProcessoUF"]/a') %>%
      xml2::xml_text()

  classe <- processo |>
           stringr::str_extract("\\p{L}+")

  processo_stj <- processo |>
         stringr::str_extract("\\d+")

  origem <- processo |>
        stringr::str_extract("\\w+$")

  tibble::tibble(registro, classe, processo_stj, origem)


  }, NULL))

}

#' @rdname stj_ler_por_publicacao
#' @export
stj_ler_por_autuacao <- stj_ler_por_publicacao
