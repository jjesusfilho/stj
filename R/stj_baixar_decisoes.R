#' Baixar decisões das urls obtidas com stj_listar_decisoes
#'
#' @param df tibble com url, registro e sequencial
#' @param diretorio Diretório onde colocar as decisões
#'
#' @return pdfs
#' @export
#'
stj_baixar_decisoes <- function(df, diretorio = "."){


  pb <- progress::progress_bar$new(total = nrow(df))


  purrr::pwalk(list(x = df$registro, y = df$sequencial, z = df$url), purrr::possibly(function(x,y,z) {

    pb$tick()

    arquivo <- file.path(diretorio, paste0("registro_", x, "_sequencial_",y, ".pdf"))

    httr::GET(z) |>
      httr::content() |>
      xml2::xml_find_first("//a") |>
      xml2::xml_attr("href") |>
      xml2::url_absolute("https://processo.stj.jus.br") |>
      httr::GET(httr::write_disk(arquivo, overwrite = TRUE))

  },NULL))


}
