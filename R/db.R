# library(DBI)
DB_CONNECTION <- NULL

getDBConnection <- function() {
  db <- "postgres"  #provide the name of your db
  host_db <- "127.0.0.1" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
  db_port <- "5432"  # or any other port specified by the DBA
  db_user <- "postgres"
  db_password <- "admin"
  if (is.null(DB_CONNECTION)) {
    message("kek")
    DB_CONNECTION <<-
      DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = db,
        host = host_db,
        port = db_port,
        user = db_user,
        password = db_password
      )
  }

  DB_CONNECTION
}


appendAccountingEntity <- function(AccountingEntity) {
  con <- getDBConnection()
  accEntity <- as.data.frame(AccountingEntity, stringsAsFactors = FALSE) %>%
    dplyr::rename("id_accounting_entity" = "id") %>%
    setNames(tolower(names(.)))

  RPostgres::dbBegin(con)
  RPostgres::dbAppendTable(con, "accounting_entity", accEntity, row.names = NULL)
  RPostgres::dbCommit(con)
}

appendFinancialStatement <- function(variables) {

}
