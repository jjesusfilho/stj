#' Baixa publicacoes do dje com base no registro e na data
#'
#' @param registro Número do registro
#' @param data DAta no formato dd/mm/yyyy
#' @param diretorio Onde baixar os pdfs
#'
#' @return pdfs
#' @export
#'
stj_baixar_publicacoes_dje <- function(registro, data, diretorio){



df <- tibble::tibble(registro, data) |>
      dplyr::distinct()

purrr::walk(df$registro, df$data, purrr::possibly(~{

registro1 <- .x |>
             stringr::str_remove_all("\\D")

registro2 <- registro1 |>
          stringr::str_replace("(\\d{4})(\\d+)(\\d$)",'\\1/\\2-\\3')


dt_pesquisa <- Sys.Date() |>
               format("%d/%m/%Y")

url <- "https://processo.stj.jus.br/processo/dj/consulta/registro"

body <-
  list(
    aplicacao = "dj.resultados",
    data_pesquisa_texto = .y,
    sel_tipo_pesquisa = "num_reg",
    parametro_tela = registro2,
    parametro = registro1,
    desc_parametro = "",
    tipo_operacao_fonetica = "C",
    nu_pagina_atual = "0",
    proximo = "TRUE",
    tipo_pesquisa = "num_reg",
    data_pesquisa = .y,
   # data_pesquisa_01 = "09/06/2023",
    data_pesquisa_fim = "",
    padrao_data = "padrao_data_publicacao",
    padrao_tela_documentos = "padrao_tela_documentos_1_1"
  )


docs <- httr::POST(url, body = body, encode = "form") |>
       httr::content() |>
  xml2::xml_find_all("//a[contains(@id,'idDjArvoreDocumentoLink')]") |>
  xml2::xml_attr("onclick")


seq_documento <- docs |>
                 stringr::str_extract("(?<=id_check_)\\d+")

seq_publicacao <- docs |>
                stringr::str_extract("(?<=impressao...)\\d+")

nu_seguimento <- docs |>
                 stringr::str_extract("\\d+(?=\\D+$)")

lista <- list(x = seq_documento, y = seq_publicacao, z= nu_seguimento)

purrr::pwalk(lista, purrr::possibly(function(x, y,z){

url <- paste0("https://processo.stj.jus.br/processo/dj/documento/?seq_documento=", x,"&data_pesquisa=", .y,"&seq_publicacao=", y,"&versao=impressao&nu_seguimento=",z)

dt <- stringr::str_replace_all(.y, "/","_")
arquivo <- file.path(diretorio, paste0("registro_", .x, "_data_",dt, "_seq_documento_", x,"_seq_publicacao_",y, "_nu_seguimento_", z,".pdf"))

httr::GET(url, httr::write_disk(arquivo, overwrite = T))

}, NULL))

}, NULL))


}
