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


#' Title
#'
#' @param x
#' @param tableName
#' @param con
#'
#' @return
#' @export
#'
#' @examples
appendIfMissing <- function(x, tableName, con) {
  whereStatement <- lapply(names(x), function(name) {
    glue::glue_sql("{`name`} NOT IN ({x[[name]]*})", .con = con)
  }) %>% unlist() %>% glue::glue_collapse(sep = " OR ")

  res <- glue::glue_sql(paste("SELECT * FROM {`tableName`}"), .con = con) %>%
    RPostgres::dbGetQuery(con, .)

  toAppend <- dplyr::anti_join(toCharDF(x), toCharDF(res[names(x)]), by = names(x))

  if (nrow(toAppend) > 0) {
    nrOfAppendedRows <- RPostgres::dbAppendTable(con, tableName, toAppend, row.names = NULL)
    message("Appended missing values to table: ", nrOfAppendedRows)
  }
}
