#' Lê urls da aba decisões baixada com stj_baixar_processo
#'
#' @param arquivos Vetor de arquivos.
#' @param diretorio Informar diretório se não informar arquivos.
#'
#' @return tibble
#' @export
#'
stj_listar_decisoes <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern  = "html$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()


    x <- xml2::read_html(.x)

    urls <- x |>
      xml2::xml_find_all("//div[@id='idDivDecisoes']//a[span]") |>
      xml2::xml_attr("onclick") |>
      stringr::str_extract("processo.+?(?=')") |>
      xml2::url_absolute("https://processo.stj.jus.br")

    fonte <- stringr::str_extract(urls, "(?<=processo/)\\w+")
    componente <- stringr::str_extract(urls, "(?<=componente=)\\w+")
    tipo_documento <- stringr::str_extract(urls,"(?<=tipo_documento=)\\w+")
    peticao_numero <- stringr::str_extract(urls, "(?<=peticao_numero=)\\d+")
    sequencial <- stringr::str_extract(urls, "(?<=sequencial=)\\d+")
    registro <- stringr::str_extract(urls, "(?<=(num_registro|numero)=)\\d+")
    data <- stringr::str_extract(urls,"(?<=data=)\\d+") |>
      lubridate::ymd()


    texto <- x |>
      xml2::xml_find_all("//div[@id='idDivDecisoes']//a[span]") |>
      xml2::xml_text()

    tibble::tibble(registro, sequencial,tipo_documento, peticao_numero, componente, fonte, url = urls, data, texto)


  },NULL))

}
