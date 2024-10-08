% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stj_ler_detalhes.R
\name{stj_ler_detalhes2}
\alias{stj_ler_detalhes2}
\title{Ler detalhes dos processos do STJ V2}
\usage{
stj_ler_detalhes2(arquivos = NULL, diretorio = ".", wide = FALSE)
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
Ler detalhes dos processos do STJ V2
}
\details{
A diferença dessa função é que ela cria uma coluna
    com o nome do arquivo.
}
\examples{
\dontrun{
df <- ler_detalhes_stj(diretorio = ".")
}
}
