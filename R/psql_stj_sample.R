#' Gets a table's sample
#'
#' @param con connection
#' @param tbl table name
#' @param column optional column name to apply filter
#' @param value optional column value to apply filter
#' @param n number of rows, defaults to 10.
#'
#' @return a data.frame sample according to specified arguements.
#' @export
#'
psql_stj_sample <- function(con, tbl = NULL, column = NULL, value = NULL, n = 10) {
  if (is.null(column)) {
    query <- glue::glue_sql("SElECT *
                      FROM {`tbl`}
                      ORDER BY random() LIMIT {n}",
                            .con = con
    )
  } else {
    query <- glue::glue_sql("SElECT *
                      FROM {`tbl`}
                      WHERE {`tbl`}.{`column`} = {value}
                      ORDER BY random() LIMIT {n}",
                            .con = con
    )
  }
  DBI::dbGetQuery(con, query)
}
