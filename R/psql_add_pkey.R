#' Adiciona chame primária a uma tabela
#'
#' @param con conexão
#' @param tbl nome da tabela
#'
#' @return Adiciona uma chave primária
#' @export
#'
#' @examples
#' \dontrun{
#' data(consumidor)
#' con <- dbx::dbxConnect()
#' dplyr::copy_to(con, "consumidor", "consumidor")
#' psql_add_pkey(con, "consumidor")
#' }
psql_add_pkey <- function(con, tbl) {
  id <- paste(tbl, "id", sep = "_")
  key <- paste(tbl, "pkey", sep = "_")
  query <- glue::glue_sql("ALTER TABLE {`tbl`} ADD COLUMN {`id`} SERIAL", .con = con)

  res <- DBI::dbSendQuery(con, query)
  DBI::dbClearResult(res)

  query <- glue::glue_sql("AlTER TABLE {`tbl`} ADD CONSTRAINT {`key`} PRIMARY KEY ({`id`})", .con = con)

  res <- DBI::dbSendQuery(con, query)


  DBI::dbClearResult(res)
}
