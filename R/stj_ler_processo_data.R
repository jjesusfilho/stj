#' Lê processos baixados com stj_baixar_processo_data
#'
#' @param arquivos Vetor de caminhos para os arquivos
#' @param diretorio Atlernativamente, informar diretório
#'
#' @return tibble
#' @export
#'
stj_ler_processo_data <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio , full.names = T, pattern = "html$")

  }

purrr::map_dfr(arquivos, purrr::possibly(~{


conteudo <- .x |>
     xml2::read_html() |>
     xml2::xml_find_all("//div[@class='clsListaProcessoFormatoVerticalBlocoExterno']")

conteudo <- conteudo[2:101]

df <- purrr::map_dfr(conteudo, purrr::possibly(~{


classe_n_uf <- .x |>
       xml2::xml_find_all(".//span[@class='clsBlocoProcessoColuna clsBlocoProcessoColuna1 classSpanProcessoUF']") |>
       xml2::xml_text()

classe <- stringr::str_extract(classe_n_uf, "\\S+")
numero <- stringr::str_extract(classe_n_uf, "\\d+")
uf <- stringr::str_extract(classe_n_uf, "\\w+$")

registro <- .x |>
  xml2::xml_find_all(".//span[@class='clsBlocoProcessoColuna clsBlocoProcessoColuna2 classSpanNumeroRegistro']") |>
  xml2::xml_text()


dt_autuacao <- .x |>
  xml2::xml_find_all(".//span[@class='clsBlocoProcessoColuna clsBlocoProcessoColuna3 clsLinhaProcessosDataAutuacao']") |>
  xml2::xml_text() |>
  lubridate::dmy(tz = "America/Sao_Paulo")


partes <- .x |>
    xml2::xml_find_all(".//span[@class='clsListaProcessoParte']")

tipo_parte <- partes |>
          purrr::map_chr(~{
            .x |>
               xml2::xml_find_first("./span[@class='clsListaProcessoParteTipo']") |>
              xml2::xml_text() |>
              stringr::str_remove(":")
          })

parte <- partes |>
  purrr::map_chr(~{
    .x |>
      xml2::xml_find_first("./span[@class='clsListaProcessoParteNome']") |>
      xml2::xml_text()
  })

partes <- tibble::tibble(tipo_parte, parte)

assuntos <- .x |>
  xml2::xml_find_all(".//span[@class='clsProcessosListaEtiquetaCabecalho'][contains(.,'Assunto')]/following-sibling::span/span") |>
  xml2::xml_text(trim = T) |>
  #stringr::str_c(collapse = ";") |>
  stringr::str_remove_all("[.,]")

ramo_do_direito <- .x |>
  xml2::xml_find_all(".//span[@class='clsProcessosListaEtiquetaCabecalho'][contains(.,'Ramo do Direito')]/following-sibling::span") |>
  xml2::xml_text()

tribunal_de_origem <- .x |>
  xml2::xml_find_all(".//span[@class='clsProcessosListaEtiquetaCabecalho'][contains(.,'Tribunal de Origem')]/following-sibling::span") |>
  xml2::xml_text()

tibble::tibble(classe, numero, uf, registro, dt_autuacao, assuntos = list(assuntos), ramo_do_direito, tribunal_de_origem, partes = list(partes))

}, NULL))

}, NULL), .progress = TRUE)

}
