#' Get all changed accounting entities.
#'
#' Get all changed accounting entities.
#'
#' @param from `character` with date since when the changes happened.
#' @param maxNumber `numeric` with the number of changed records that should be pulled.
#' @param afterId `numeric` after which ID should we get changed records.
#'
#' @return `list` with changed records.
getChangedAccountingEntities <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-jednotky", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = format(afterId, scientific = FALSE)) %>%
    tryUntilSuccess()
}

#' Get all changed financial statements
#'
#' Get all changed financial statements.
#'
#' @inheritParams getChangedAccountingEntities
#'
#' @return  with changed records.
getChangedFinancialStatements <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-zavierky", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = format(afterId, scientific = FALSE)) %>%
    tryUntilSuccess()
}

#' Get all changed financial reports
#'
#' Get all changed financial reports.
#'
#' @inheritParams getChangedAccountingEntities
#'
#' @return with changed records.
getChangedFinancialReports <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-vykazy", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = format(afterId, scientific = FALSE)) %>%
    tryUntilSuccess()
}

#' Get details of accounting entity
#'
#' Get details of accounting entity.
#'
#' @param id `numeric` id of the entity.
#'
#' @return `list` with details.
getAccountingEntityDetails <- function(id) {
  createUrl("/uctovna-jednotka", id = format(id, scientific = FALSE)) %>%
    tryUntilSuccess()
}

#' Get details of financial statement
#'
#' Get details of financial statement.
#'
#' @param id `numeric` id of the statement.
#'
#' @return `list` with details.
getFinancialStatementDetails <- function(id) {
  createUrl("/uctovna-zavierka", id = format(id, scientific = FALSE)) %>%
    tryUntilSuccess()
}

#' Get details of financial report
#'
#' Get details of financial report.
#'
#' @param id `numeric` id of the report.
#'
#' @return `list` with details.
getFinancialReportDetails <- function(id) {
  createUrl("/uctovny-vykaz", id = format(id, scientific = FALSE)) %>%
    tryUntilSuccess()
}
