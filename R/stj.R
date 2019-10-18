#' \code{stj} package
#'
#' Baixa  e organiza decisões do STJ
#'
#'
#' @docType package
#' @name stj
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sequencial",".",".x",".y"))
}
