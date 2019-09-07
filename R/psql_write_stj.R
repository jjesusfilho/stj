#' Cria e salva uma tabela no PostgreSQL
#'
#' @description Esta função cria uma tabela com base
#'     nas colunas a serem salvas. Em seguida adiciona
#'     uma chave primária chamada document_ix. Depois
#'     disso insere as observações. Ao final, cria um
#'     índice remissivo chamado document_tokens com 
#'     base no algorítimo GIN.
#' 
#' 
#'
#' @param con a connection
#' @param tbl table name
#' @param data stj data.frame to be written.
#'
#' @return returns NULL if everything went well
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbx::dbxConnect()
#' psql_write_stj(con, "consumidor", consumidor)
#' }
psql_write_stj <- function(con = NULL, tbl = NULL, data = NULL) {
  if (is.null(con)) {
    stop("Please provide a connection")
  }
  
  if (is.null(tbl)) {
    stop("You must provide an existing table to insert data")
  }
  
  if (is.null(data)) {
    stop("You must provide the data to be inserted")
  }
  
  DBI::dbCreateTable(con, tbl, data)
  
  psql_add_pkey(con, tbl)
  psql_stj_insert(con, tbl, data = data)
  psql_stj_tokenize(con, tbl)
  psql_stj_trigger(con, tbl)
}


