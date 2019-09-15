#' Lê julgados do STJ, incluindo todos os campos
#'
#' @param arquivos lista de arquivos
#' @param diretorio se arquivos não forem informados,
#'     informar diretório
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_julgados_stj2()
#' }

ler_julgados_stj2 <- function(arquivos = NULL,
                              diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, "html", full.names = TRUE)
  }
  purrr::map_dfr(arquivos, purrr::possibly(~{

    principal <- xml2::read_html(.x)

    principal %>%
      xml2::xml_find_all("//div[@style='position: relative;'][child::div[@class='paragrafoBRS']]") %>%
      purrr::map(xml2::xml_children) %>%
      purrr::map_dfr(~xml2::xml_text(.x) %>%
                       stringr::str_trim() %>%
                       stringr::str_split("(\r\n\t?)+",simplify =T) %>%
                       tibble::as_tibble() %>%
                       dplyr::slice(-c(1:3)) %>%
                       dplyr::select(1:2) %>%
                       tidyr::pivot_wider(names_from = V1,values_from =V2) %>%
                       janitor::clean_names() %>%
                       dplyr::mutate(registro = stringr::str_extract(processo,"\\d{4}/\\d+-\\d")) %>%
                       dplyr::rename(processo_stj = "processo") %>%
                       dplyr::mutate(processo_stj = stringr::str_extract(processo_stj,".+?(?=/)")) %>%
                       dplyr::select(registro, dplyr::everything())) %>%
      dplyr::mutate_all(stringr::str_squish) %>%
      dplyr::rename(data_julgamento = "data_do_julgamento",
                    data_publicacao = "data_da_publicacao_fonte",
                    relator = "relator_a") %>%
      dplyr::mutate(data_publicacao = lubridate::dmy(data_publicacao),
                    data_julgamento = lubridate::dmy(data_julgamento),
                    relator = stringr::str_remove(relator,"(?i)Min\\w+\\s?")) %>%
      tidyr::separate(relator, c("relator","numero_relator"),sep=" \\(") %>%
      dplyr::mutate(numero_relator = stringr::str_remove_all(numero_relator,"\\D+"))


  }, NULL))
}

