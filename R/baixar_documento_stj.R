#' Baixar documentos do processo, ex. decisões, certidões etc
#'
#' @param sequencial códigos dos documentos obtido com ler_metadocs_stj
#' @param plano ver `future::plan`
#' @param diretorio se não informado, diretório atual
#' @param formato PDF ou HTML
#'
#' @return htmls ou pdfs
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_documento_stj("1764491")
#' }
baixar_documento_stj <- function(sequencial,  diretorio = ".", formato=c("PDF","HTML"))
  {

  formato <- toupper(formato) %>%
             `[`(1)

  url<-paste0("https://ww2.stj.jus.br/websecstj/cgi/revista/REJ.cgi/ATC?seq=",sequencial,"&tipo=0&nreg=&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=&formato=",formato,"&salvar=false")



   purrr::walk2(url,sequencial, purrr::possibly(~{

    arquivo <- paste0("_sequencial_",.y,".",tolower(formato))

    httr::RETRY("GET", url = .x, httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo))))


  },NULL))

}
