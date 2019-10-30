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
    dplyr::mutate(parte = stringr::str_replace(parte,"^$","v1")) %>%
    tidyr::pivot_wider(names_from = parte, values_from=parte_nome) %>%
    dplyr::select(-c(partes_id,numero)) %>%
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

  extra_dados<-DBI::dbGetQuery(con,query) %>%
    dplyr::select(registro,extra=variavel,info_extra=valor)  %>%
    tidyr::pivot_wider(names_from=extra,values_from = info_extra)

  detalhes <- "detalhes"

  query<- glue::glue_sql("
select *
  from {`detalhes`}
  WHERE {`detalhes`}.registro IN ({df$registro*})
",.con=con)

  detalhes<-DBI::dbGetQuery(con,query) %>%
    dplyr::select(registro,detalhe=variavel, detalhe_valor=valor)

  detalhes <- detalhes %>%
    dplyr::mutate(detalhe = stringr::str_replace(detalhe,"^$","v1")) %>%
    tidyr::pivot_wider(names_from=detalhe,values_from=detalhe_valor) %>%
    janitor::clean_names() %>%
    dplyr::mutate(v1=NULL)

  dados %>%
    dplyr::left_join(partes, by='registro') %>%
    dplyr::left_join(detalhes, by="registro") %>%
    dplyr::left_join(extra_dados, by = "registro") %>%
    dplyr::left_join(df,by="registro") %>%
    dplyr::mutate(julgado = stringr::str_replace_all(julgado,"\n","<br>"))


}
