#' Baixa documents a partir do tibble criado com ler_metadocs_stj
#'
#' @param df Tibble
#' @param diretorio Diretório onde armazenar os arquivos
#'
#' @return pdfs
#' @export
#'
stj_baixar_documentos_pdf <- function(df, diretorio = "."){

df <- stats::na.omit(df)

pb <- progress::progress_bar$new(total = nrow(df))

purrr::pwalk(df, purrr::possibly(function(sequencial, registro, peticao_numero, data_publicacao,...){

  pb$tick()

url <- paste0("https://processo.stj.jus.br/processo/julgamento/eletronico/documento/?documento_tipo=integra&documento_sequencial=",sequencial,"&registro_numero=",registro,"&peticao_numero=",peticao_numero,"&publicacao_data=",data_publicacao,"&formato=PDF")

data <- stringr::str_replace_all(Sys.time(),"\\D","_")

arquivo <- file.path(diretorio,paste0(data,"_registro_",registro,"_sequencial_",sequencial,"_peticao_numero_",peticao_numero,"_data_publicacao_",data_publicacao,".pdf"))

httr::GET(url,httr::write_disk(arquivo, overwrite = TRUE))

},NULL))

}
