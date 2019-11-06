#' PostgreSQL full-text search
#'
#' @param con connection
#' @param tbl table
#' @param assuntos Vetor de assuntos
#' @param classes vetor de classes
#' @param registros vetor de registros
#' @param origem vetor de origem
#' @param start data inicial no formato "yyyy-mm-dd"
#' @param end data final no formato "yyyy-mm-dd"
#' @param query palavras a serem buscadas
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dplyr::copy_to(con, "julgados", df)
#' df <- psql_stj_query(con, "julgados", "IRPF")
#' }
psql_stj_query <- function(con, tbl, query = "",assuntos = NULL,classes = NULL, registros = NULL, origem = NULL,start = "2009-01-01", end = NULL) {
  target <- "document_tokens"

  if (is.null(end)) end <- Sys.Date()

  start <- as.Date(start)

  if (!is.null(assuntos) & !is.null(classes) & !is.null(registros) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.assunto IN ({assuntos*})
                        AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.registro IN ({registros*})
                        AND {`tbl`}.tribunal_de_origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)

  } else if (is.null(assuntos) & !is.null(classes) & !is.null(registros) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.registro IN ({registros*})
                        AND {`tbl`}.tribunal_de_origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)

  } else if (!is.null(assuntos) & is.null(classes) & !is.null(registros) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.assunto IN ({assuntos*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.registro IN ({registros*})
                        AND {`tbl`}.tribunal_de_origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)

  } else if (!is.null(assuntos) & !is.null(classes) & is.null(registros) & !is.null(origem) & query != "" ){

    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.assunto IN ({assuntos*})
                        AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.tribunal_de_origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)

  } else if (!is.null(assuntos) & !is.null(classes) & !is.null(registros) & is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.assunto IN ({assuntos*})
                        AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.registro IN ({registros*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)


  } else if (!is.null(assuntos) & !is.null(classes) & !is.null(registros) & is.null(origem) & query == "" ) {
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.assunto IN ({assuntos*})
                        AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.registro IN ({registros*})
                        AND {`tbl`}.tribunal_de_origem IN ({origem*})", .con = con)

  } else

    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.{`target`} @@ websearch_to_tsquery({query})", .con = con)


  df <- DBI::dbGetQuery(con, q) %>%
    dplyr::select(registro,documento,julgado)

  partes <- "partes"
  query<- glue::glue_sql("
select *
  from {`partes`}
  WHERE {`partes`}.registro IN ({df$registro*})
",.con=con)

  partes<-DBI::dbGetQuery(con,query) %>%
    dplyr::select(-c(partes_id)) %>%
    janitor::clean_names()


  dados<-"dados"
  query<- glue::glue_sql("
select *
  from {`dados`}
  WHERE {`dados`}.registro IN ({df$registro*})
",.con=con)


  dados<-DBI::dbGetQuery(con,query) %>%
    dplyr::select(-dados_id)

  extra_dados <- "extra_dados"

  query<- glue::glue_sql("
select *
  from {`extra_dados`}
  WHERE {`extra_dados`}.registro IN ({df$registro*})
",.con=con)

  extra_dados<-DBI::dbGetQuery(con,query)

  detalhes <- "detalhes"

  query<- glue::glue_sql("
select *
  from {`detalhes`}
  WHERE {`detalhes`}.registro IN ({df$registro*})
",.con=con)

  detalhes<-DBI::dbGetQuery(con,query)

  tudo_na<- function(x){
    all(is.na(x))
  }

  dd<-  dados %>%
    dplyr::left_join(partes) %>%
    dplyr::left_join(detalhes) %>%
    dplyr::left_join(extra_dados) %>%
    dplyr::left_join(df) %>%
    dplyr::mutate(julgado = stringr::str_replace_all(julgado,"\n","<br>"))

  dd[, colSums(is.na(dd)) != nrow(dd)]

}
