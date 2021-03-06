% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrair_metadados_stj.R
\name{extrair_metadados_stj}
\alias{extrair_metadados_stj}
\title{Extrair metadados da jurisprudência do STJ}
\usage{
extrair_metadados_stj(
  livre = "",
  aspas = FALSE,
  data_inicial = "",
  data_final = ""
)
}
\arguments{
\item{livre}{Se deixar em branco, informe as datas.}

\item{aspas}{Colocar entre aspas a busca}

\item{data_inicial}{Data inicial no formato "dd/mm/aaaa"}

\item{data_final}{Data final no formato "dd/mm/aaaa"}
}
\value{
tibble com metadados
}
\description{
Extrair metadados da jurisprudência do STJ
}
\examples{
\dontrun{
df <- extrair_metadados_stj(
   data_inicial = "01/07/2019",
    data_final = "30/07/2019"
    )
}
}
