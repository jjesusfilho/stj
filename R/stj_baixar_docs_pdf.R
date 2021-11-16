#' Baixa docs em pdf
#'
#' @param sequencial Sequencial do documento
#' @param registro  Número do registro
#' @param data Data no formato dd/mm/aaaa
#' @param diretorio Diretório onde armazenar os pdfs
#'
#' @return pdf
#' @export
#'
#' @examples
#' \dontrun{
#' stj_baixar_docs_pdf("112282908", "20200165980","04/08/2020")
#'
#' }
stj_baixar_docs_pdf <- function(sequencial, registro, data, diretorio  = "."){

  registro <- registro |>
    stringr::str_remove("\\D")

  data <- data |>
    lubridate::dmy() %>%
    stringr::str_remove_all("\\D")


  purrr::pwalk(list(x = sequencial, y = registro, z = data), purrr::possibly(function(x,y,z) {

    u <- paste0("https://www.stj.jus.br/websecstj/cgi/revista/REJ.cgi/MON?seq=",x,"&tipo=0&nreg=",y,"&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=", z, "&formato=PDF&salvar=false")

    arquivo <- file.path(diretorio, paste0("stj_doc_sequencial_",x, "_registro_", y, "_data_", z,".pdf"))

    httr::GET(u, httr::write_disk(arquivo, overwrite = TRUE))


  },NULL))

}
