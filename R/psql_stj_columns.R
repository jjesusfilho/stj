#' psql_columns
#'
#' @param con connection
#' @param tbl table name
#'
#' @return a data.frame with 18 columns describing the table columns.
#' @export
#'
psql_columns <- function(con, tbl = NULL) {
  query <- glue::glue_sql(
    "SELECT *
                         FROM information_schema.columns
                         WHERE table_schema = 'public'
                         AND table_name   = {tbl}
                        ",
    .con = con
  )
  
  DBI::dbGetQuery(con, query)
}
