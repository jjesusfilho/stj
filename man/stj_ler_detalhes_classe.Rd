% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stj_ler_detalhes_classe.R
\name{stj_ler_detalhes_classe}
\alias{stj_ler_detalhes_classe}
\title{Ler detalhes dos processos do STJ baixados com stj_baixar_classe_processo}
\usage{
stj_ler_detalhes_classe(arquivos = NULL, diretorio = ".", wide = FALSE)
}
\arguments{
\item{arquivos}{Se os caminhos para os arquivos forem fornecidos
o diretório é ignorado.}

\item{diretorio}{Diretório se arquivos não forem informados}

\item{wide}{Colocar no formato largo? Padrão é longo.}
}
\value{
tibble com detalhes do processo
}
\description{
Ler detalhes dos processos do STJ baixados com stj_baixar_classe_processo
}
\examples{
\dontrun{
df <- stj_ler_detalhes_classe(diretorio = ".")
}
}
