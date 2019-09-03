#' Baixar decisões do processo
#'
#' @param sequencial códigos dos documentos
#' @param plano ver `future::plan`
#' @param diretorio se não informado, diretório atual
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_pdf_stj("1764491")
#' }
baixar_pdfs_stj <- function(sequencial, plano = "sequential", diretorio = "."){

  url<-paste0("https://ww2.stj.jus.br/websecstj/cgi/revista/REJ.cgi/ATC?seq=",sequencial,"&tipo=0&nreg=&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=&formato=PDF&salvar=false")

  future::plan(plano)

  furrr::future_map2(url,sequencial, purrr::possibly(~{

    arquivo <- paste0("_sequencial_",.y,".pdf")

    httr::RETRY("GET", url = .x, httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo))))


  },NULL))

}
