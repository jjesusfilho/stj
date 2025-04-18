#' Baixar decisões das urls obtidas com stj_listar_decisoes
#'
#' @param df tibble com url, registro e sequencial
#' @param diretorio Diretório onde colocar as decisões
#'
#' @return pdfs
#' @export
#'
stj_baixar_decisoes <- function(df, diretorio = "."){

  httr::set_config(httr::config(ssl_verifypeer = 0L))

 
  purrr::pwalk(list(x = df$registro, y = df$sequencial, z = df$url), purrr::possibly(function(x,y,z) {

    arquivo <- file.path(diretorio, paste0("registro_", x, "_sequencial_",y, ".pdf"))


    r1 <- httr::GET(z, httr::add_headers(`istl-infinite-loop` = 1))

    if(httr::http_type(r1) != "application/pdf"){

      r1 |>
        httr::content() |>
        xml2::xml_find_first("//a") |>
        xml2::xml_attr("href") |>
        xml2::url_absolute("https://processo.stj.jus.br") |>
        httr::GET(httr::write_disk(arquivo, overwrite = TRUE),httr::add_headers(`istl-infinite-loop` = 1))

    } else {

      writeBin(r1$content,arquivo)
    }


  },NULL), .progress = TRUE)
}
