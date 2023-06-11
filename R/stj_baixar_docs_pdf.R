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
    stringr::str_remove_all("\\D")

  data <- data |>
    lubridate::dmy() %>%
    stringr::str_remove_all("\\D")

  httr::set_config(httr::config(ssl_verifypeer = 0L))

  purrr::pwalk(list(x = sequencial, y = registro, z = data), purrr::possibly(function(x,y,z) {

    u1 <- paste0("https://www.stj.jus.br/websecstj/cgi/revista/REJ.cgi/MON?seq=",x,"&tipo=0&nreg=",y,"&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=", z, "&formato=PDF&salvar=false")
    u2 <- paste0("https://processo.stj.jus.br/processo/dj/documento/?&sequencial=", x,"&num_registro=",y,"&data=",z, "&formato=PDF&componente=MON")
    u3 <- paste0("https://ww2.stj.jus.br/websecstj/cgi/revista/REJ.cgi/ITA?seq=",x,"&tipo=0&nreg=",y, "&SeqCgrmaSessao=&CodOrgaoJgdr=&dt=",z, "&formato=PDF&salvar=false")
    r1 <- httr::GET(u1)
    r2 <- httr::GET(u2)
    r3 <- curl::curl_fetch_memory(u3)

    rs <- list(r1, r2, r3)

    tipo1 <- r1$headers$`content-type`
    tipo2 <- r2$headers$`content-type`
    tipo3 <- r3$type

    pdf <- "application/pdf"

    pdfs <- is.element(c(tipo1, tipo2, tipo3), pdf)

    tamanho1 <- r1$headers$`content-length` |> as.integer()
    tamanho2 <- r2$headers$`content-length` |> as.integer()
    tamanho3 <- r3$headers |>
               rawToChar() |>
               stringr::str_squish() |>
               stringr::str_extract_all("(?<=Content-Length: )\\d+") |>
               unlist()|>
               as.integer() |>
               max()




    if (all(pdfs)){

    r <-   rs[which.max(c(tamanho1, tamanho2, tamanho3))][[1]]

    } else {

    r <- rs[which(pdfs)][[1]]

    }




    arquivo <- file.path(diretorio, paste0("stj_doc_sequencial_",x, "_registro_", y, "_data_", z,".pdf"))

    writeBin(r$content, arquivo)


  },NULL))

}
