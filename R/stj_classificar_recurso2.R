#' Classifica recurso com apenas vetor de julgados
#'
#' @param x Julgado ou dispositivo
#' @details Esta função se diferencia da outra porque sua entrada
#'     é um vetor e não um data.frame. Mais tarde, irei convertê-las
#'     em métodos de um mesmo genérico.
#' @return classificacao (string)
#' @export
#'
stj_classificar_recurso2 <- function (x)
{

  x<- x |>
    tolower() |>
    stringi::stri_trans_general("latin-ascii")

  dplyr::case_when(
    stringi::stri_detect_regex(x, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
    stringi::stri_detect_regex(x, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
    stringi::stri_detect_regex(x, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
    stringi::stri_detect_regex(x, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
    stringi::stri_detect_regex(x, "parcial\\w*\\sprovi\\w+") ~ "parcial",
    stringi::stri_detect_regex(x, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
    stringi::stri_detect_regex(x, "desconh\\w+") ~ "desconhecido",
    stringi::stri_detect_regex(x, "nao\\s+conhec\\w+") ~ "desconhecido",
    stringi::stri_detect_regex(x, "rejeit\\w+ (os)? embargos") ~ "embargos rejeitados",
    stringi::stri_detect_regex(x, "rejeitam-se (os)? embargos") ~ "embargos rejeitados",
    stringi::stri_detect_regex(x, "(desprov\\w+|improv\\w+)") ~ "improvido",
    stringi::stri_detect_regex(x, "(nao|nega\\w+)\\s+provi\\X*") ~ "improvido",
    stringi::stri_detect_regex(x, "(nego\\w+)\\s+provi\\X*") ~ "improvido",
    stringi::stri_detect_regex(x, "provi\\w+") ~ "provido",
    stringi::stri_detect_regex(x, "mantiveram") ~ "improvido",
    stringi::stri_detect_regex(x, "acolh\\w+") ~ "provido",
    stringi::stri_detect_regex(x, "(deu|deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)") ~ "provido",
    stringi::stri_detect_regex(x, "(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)") ~ "improvido",
    stringi::stri_detect_regex(x, "(homolog|desistencia)") ~ "desist\u00eancia",
    stringi::stri_detect_regex(x, "rejeit\\w+ (os)? embargos") ~ "embargos rejeitados",
    stringi::stri_detect_regex(x, "(anular\\w*|nulo|nula|nulidade)") ~ "anulado",
    stringi::stri_detect_regex(x, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
    stringi::stri_detect_regex(x, "(prej|extin)") ~ "prejudicado/extinto",
    TRUE ~ NA_character_
  )
}
