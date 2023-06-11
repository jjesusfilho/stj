#' Baixa decisões do stj com base no registro  e no sequencial.
#'
#' @param registro Número do registro
#' @param sequencial Número do sequencial
#' @param diretorio Caminho
#'
#' @return pdfs
#' @export
#'
stj_baixar_decisoes2 <- function(registro,sequencial, diretorio = "."){

  httr::set_config(httr::config(ssl_verifypeer = 0L))

  pb <- progress::progress_bar$new(total = length(sequencial))

  purrr::walk2(registro, sequencial, purrr::possibly(~{

                 pb$tick()
                 arquivo <- file.path(diretorio, paste0("registro_",
                                                        .x, "_sequencial_", .y, ".pdf"))

                 url <- paste0("https://processo.stj.jus.br/processo/dj/documento/?=&sequencial=", .y,"&num_registro=",.x)

                 httr::GET(url, httr::write_disk(url, arquivo, overwrite = T))

                 }, NULL))



}
