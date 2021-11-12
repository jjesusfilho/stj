#' Classifica recursos
#'
#' @param x data.frame com a coluna a ser classificada
#' @param dispositivo Coluna onde se encontram os dispositivos do accórdão
#' @param decisao Nome da coluna a ser criada com a decisão
#'
#' @return x adicionado da coluna decisão.
#' @export
#'
stj_classificar_recurso <- function(x, dispositivo, decisao) {

  ## Para reduzir o tempo de computa\u00e7\u00e3o, eu optei por dar um count na coluna a ser classificada.
  ## Poderia usar somente a coluna, mas count rearranja por ordem alfab\u00e9tica, ou por outra ordem,
  ## mesmo que eu d\u00ea sort=FALSE. Diante dessa limita\u00e7\u00e3o, eu inclu\u00ed o data.frame como input, para
  ## mais tarde dar um left_join. Alternativamente, pode-se converter o vetor original em tibble
  ## e dar um left_join mais tarde. N\u00e3o sei o que \u00e9 melhor.

  input <- rlang::enexpr(dispositivo)
  decisao_out <- rlang::enexpr(decisao)
  y <- x %>%
    dplyr::distinct(!!input) %>%
    dplyr::mutate(alternativa = tolower(!!input) %>%
                    stringi::stri_trans_general(., "latin-ascii"))

  y <- y %>%
    dplyr::mutate(!!decisao_out :=
                    dplyr::case_when(
                      stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
                      stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
                      stringi::stri_detect_regex(alternativa, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
                      stringi::stri_detect_regex(alternativa, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
                      stringi::stri_detect_regex(alternativa, "parcial\\w*\\sprovi\\w+") ~ "parcial",
                      stringi::stri_detect_regex(alternativa, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
                      stringi::stri_detect_regex(alternativa, "^desconh\\w+") ~ "desconhecido",
                      stringi::stri_detect_regex(alternativa, "nao\\s+conhec\\w+") ~ "desconhecido",
                      stringi::stri_detect_regex(alternativa, "^(desp|impr)") ~ "improvido",
                      stringi::stri_detect_regex(alternativa, "(nao|nega\\w+)\\s+provi\\X*") ~ "improvido",
                      stringi::stri_detect_regex(alternativa, "^prove\\w+") ~ "provido",
                      stringi::stri_detect_regex(alternativa, "^mantiveram") ~ "improvido",
                      stringi::stri_detect_regex(alternativa, "acolh\\w+") ~ "provido",
                      stringi::stri_detect_regex(alternativa, "(deu|deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece|dar\\-lhe\\s*|dar)") ~ "provido",
                      stringi::stri_detect_regex(alternativa, "(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)") ~ "improvido",
                      stringi::stri_detect_regex(alternativa, "(homolog|desistencia)") ~ "desist\u00eancia",
                      stringi::stri_detect_regex(alternativa, "(anular\\w*|nulo|nula|nulidade)") ~ "anulado",
                      stringi::stri_detect_regex(alternativa, "receber\\s*a\\s*denuncia") ~ "den\u00fancia recebida",
                      stringi::stri_detect_regex(alternativa, "receber.{1,20}denuncia") ~ "den\u00fancia parcialmente recebida",
                      stringi::stri_detect_regex(alternativa, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
                      stringi::stri_detect_regex(alternativa, "(prej|extin)") ~ "prejudicado/extinto",
                      TRUE ~ "outros"
                    )) %>%
    dplyr::select(-alternativa)

  x %>%
    dplyr::left_join(y)
}
