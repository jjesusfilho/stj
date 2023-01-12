#' Baixar documentos do processo, ex. decisões, certidões etc
#'
#' @param sequencial códigos dos documentos obtidos com ler_metadocs_stj
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
stj_baixar_documento <- function(sequencial,  diretorio = ".", formato=c("PDF","HTML"))
  {

  formato <- toupper(formato) %>%
             `[`(1)

  url<-paste0("https://processo.stj.jus.br/websecstj/cgi/revista/REJ.cgi/ATC?seq=",sequencial,"&tipo=0&nreg=&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=&formato=",formato,"&salvar=false")

  httr::set_config(httr::config(ssl_verifypeer = 0L))


   purrr::walk2(url,sequencial, purrr::possibly(~{

    arquivo <- paste0("_sequencial_",.y,".",tolower(formato))

    httr::RETRY("GET", url = .x, httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo))))


  },NULL))

}
