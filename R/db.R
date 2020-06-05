DB_CONNECTION <- NULL

#' Get DB connection
#'
#' Returns existing DB connection, if the connection does not exist yet, it is created.
#'
#' @return DB connection
getDBConnection <- function() {
  db <- "postgres"
  host_db <- "127.0.0.1"
  db_port <- "5432"
  db_user <- "postgres"
  db_password <- "admin"
  if (is.null(DB_CONNECTION)) {
    message("Opening new DB connection")
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


#' Append new accounting entity data
#'
#' @param accEntity
#'
#' @return
#' @export
#'
#' @examples
appendAccountingEntity <- function(accEntity) {
  con <- getDBConnection()
  accEntity <- as.data.frame(accEntity, stringsAsFactors = FALSE) %>%
    dplyr::rename("id_accounting_entity" = "id") %>%
    setNames(tolower(names(.)))


  financialStatementsIDs <- accEntity$iductovnychzavierok

  finStatementsForAccEntitiy <- data.frame(
      id_accounting_entity = as.character(accEntity$id_accounting_entity),
      id_financial_statement = as.character(financialStatementsIDs),
      stringsAsFactors = FALSE
    )

  accEntity <-  accEntity[,colnames(accEntity) != "iductovnychzavierok"] %>% unique()

  RPostgres::dbBegin(con)
  appendIfMissing(accEntity, "accounting_entity", con)
  appendIfMissing(finStatementsForAccEntitiy, "financial_statement_for_accounting_entity", con)
  RPostgres::dbCommit(con)
}


#' Append new financial statement data
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
appendFinancialStatement <- function(finStatement) {
  con <- getDBConnection()
  finStatement <- as.data.frame(finStatement, stringsAsFactors = FALSE) %>%
    dplyr::rename("id_financial_statement" = "id") %>%
    setNames(tolower(names(.)))

  financialReportsIDs <- finStatement$iductovnychvykazov

  finReportsForFinStatement <- data.frame(
    id_financial_report = financialReportsIDs,
    id_financial_statement = finStatement$id_financial_statement,
    stringsAsFactors = FALSE
  )

  finStatementForAccEntity <- finStatement %>%
    dplyr::rename("accounting_entity" = "iduj") %>%
    .[c("accounting_entity", "id_financial_statement")] %>%
    toCharDF()

  finStatement <-  finStatement[,colnames(finStatement) != "iductovnychvykazov"] %>% unique()
  finStatement$obdobieod <- addDayToDate(finStatement$obdobieod)
  finStatement$obdobiedo <- addDayToDate(finStatement$obdobiedo)

  RPostgres::dbBegin(con)
  appendIfMissing(finStatement, "financial_statement", con)
  appendIfMissing(finReportsForFinStatement, "financial_report_for_financial_statement", con)
  appendIfMissing(finStatementForAccEntity, "financial_statement_for_accounting_entity", con)
  RPostgres::dbCommit(con)
}


appendIfMissing <- function(x, tableName, con) {
  whereStatement <- lapply(names(x), function(name) {
    glue::glue_sql("{`name`} NOT IN ({x[[name]]*})", .con = con)
  }) %>% unlist() %>% glue::glue_collapse(sep = " OR ")

  res <- glue::glue_sql(paste("SELECT * FROM {`tableName`} WHERE", whereStatement), .con = con) %>%
    RPostgres::dbGetQuery(con, .)

  toAppend <- dplyr::left_join(x, res[names(x)], by = names(x))
  if (nrow(toAppend) > 0) {
    message("Appending missing values to table: ", tableName)
    RPostgres::dbAppendTable(con, tableName, toAppend, row.names = NULL)
  }
}
