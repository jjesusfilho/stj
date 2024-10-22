#' Classificar writs(HC, MS, revisão criminal)
#'
#' @param x Texto da decisão
#' @param preprocessado Padrão para TRUE.  Se FALSE irá
#'   remover espaços extras, colocar em caixa baixa e
#'   remover acentos.
#' @return classificação
#' @export
#'
stj_classificar_writ <- function(x, preprocessado = TRUE){

  if (!preprocessado){

    x <- x |>
      stringr::str_squish() |>
    tolower() |>
      stringi::stri_trans_general("latin-ascii")

  }

  dplyr::case_when(
    stringr::str_detect(x, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
    stringr::str_detect(x, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
    stringr::str_detect(x, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
    stringr::str_detect(x, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
    re2::re2_detect(x, "expeco a ordem") ~ "concedido",
    re2::re2_detect(x,"\\boficio") ~ "de oficio",
    re2::re2_detect(x,"nao concedo") ~ "denegado",
    re2::re2_detect(x, "concedo")  ~ "concedido",
    re2::re2_detect(x, "\\bdefiro")  ~ "deferido",
    re2::re2_detect(x, "indefiro") ~ "indeferido",
    re2::re2_detect(x,"(prej|extin)") ~ "prejudicado/extinto",
    re2::re2_detect(x,"(indef|inder\\w+)") ~ "indeferido",
    re2::re2_detect(x,"\\bdefer\\w+") ~ "concedido",
    re2::re2_detect(x,",\\s+deferi\\w+") ~ "concedido",
    re2::re2_detect(x,"(desprov|improv)") ~ "denegado",
    re2::re2_detect(x, "parcial\\w*\\sdeferi\\w+") ~ "parcial",
    re2::re2_detect(x,"(nao|nega\\w+)\\s+provi.*") ~ "improvido",
    re2::re2_detect(x,"\\bprovid") ~ "concedido",
    re2::re2_detect(x,"parcial\\sprov\\w+") ~ "concedido",
    re2::re2_detect(x,"absolv\\w+") ~ "concedido",
    re2::re2_detect(x, "acolher\\w+|\\bprocedente") ~ "concedido",
    re2::re2_detect(x,"de*neg") ~ "denegado",
    re2::re2_detect(x,"^conce\\w+") ~ "concedido",
    re2::re2_detect(x,"^conhe\\w+") ~ "concedido",
    re2::re2_detect(x, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
    re2::re2_detect(x,"^desconh\\w+") ~ "desconhecido",
    re2::re2_detect(x, "neg\\w+ seguimento") ~  "nego seguimento",
    re2::re2_detect(x,"(homolog|desistencia)") ~ "desist\u00eancia",
    re2::re2_detect(x, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
    re2::re2_detect(x,"(,|e|votos)\\s+conce\\w+") ~ "concedido",
    re2::re2_detect(x,"solicitem-se informacoes") ~ "solicitacao de informacoes",
    TRUE ~ NA_character_
  )

}

