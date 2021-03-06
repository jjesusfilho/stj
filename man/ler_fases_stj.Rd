% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ler_fases_stj.R
\name{ler_fases_stj}
\alias{ler_fases_stj}
\title{Ler fases dos processos do STJ}
\usage{
ler_fases_stj(diretorio = ".", arquivos = NULL)
}
\arguments{
\item{diretorio}{Diretório se arquivos não forem informados}

\item{arquivos}{Se os caminhos para os arquivos forem fornecidos
o diretório é ignorado.}
}
\value{
tibble com fases do processo
}
\description{
Ler fases dos processos do STJ
}
\examples{
\dontrun{
df <- ler_fases_stj(diretorio = ".")
}
}
