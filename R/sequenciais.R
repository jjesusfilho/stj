#' Extrai os números sequenciais dos htmls obtidos com baixar_processo_trf1.
#'
#' @param arquivos Se nulo, informar o diretório
#' @param diretorio Informar se os arquivos não foram informados
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- sequenciais()
#' }
sequenciais<-function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, "_\\d{2}_(processo|registro)",full.names = TRUE)

  }



  purrr::map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"(?<=stj_)\\d.+(?=\\.html)") %>%
      stringr::str_remove_all("\\D+")

    x <- xml2::read_html(.x)

    nome<-xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_text("onclick")

    sequencial <- xml2::xml_find_all(x,"//*[contains(@onclick,'sequencial')]") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("(?<=sequencial=)\\d+")

 d<-   tibble::tibble(processo,nome, sequencial) %>%
      dplyr::distinct()

  },NULL))
}
