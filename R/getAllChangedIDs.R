#' Get all changed IDs since date
#'
#' Get all changed IDs since date.
#'
#' @param from `character` date
#' @param maxNumber `numeric` max number which should be downloaded in one batch.
#' @param fun `function` for getting IDs.
#'
#' @return `numeric` `vector` with changed IDs
getAllChangedIDs <- function(from, maxNumber = 1000, fun) {
  number <- 1
  i <- 0
  completeList <- list()
  while (TRUE) {
    result <- fun(from, maxNumber, number)
    message("Downloaded number: ", i,"| Still remaining: ", result$pocetZostavajucichId)
    number <- result$id[length(result$id)]
    completeList[[as.character(i)]] <- data.frame(result$id)
    if (result$pocetZostavajucichId <= 0) {
      break
    }
    i <- i + 1
  }
  res <- completeList %>% dplyr::bind_rows() %>% .$result.id
  res
}


#' @describeIn getAllChangedIDs
#' Get all changed financial reports.
#' @export
getAllChangedFinancialReports <- function(from, maxNumber = 1000) {
  getAllChangedIDs(from, maxNumber, getChangedFinancialReports)
}


#' @describeIn getAllChangedIDs
#' Get all changed financial statements.
#' @export
getAllChangedFinancialStatements <- function(from, maxNumber = 1000) {
  getAllChangedIDs(from, maxNumber, getChangedFinancialStatements)
}


#' @describeIn getAllChangedIDs
#' Get all changed accounting entities.
#' @export
getAllChangedAccountingEntities <- function(from, maxNumber = 1000) {
  getAllChangedIDs(from, maxNumber, getChangedAccountingEntities)
}
