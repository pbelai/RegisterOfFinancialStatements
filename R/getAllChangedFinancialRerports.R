getAllChangedFinancialReports <- function(from, maxNumber = 1000) {
  number <- 1
  i <- 0
  completeList <- list()
  while (TRUE) {
    result <- getChangedFinancialReports(from, maxNumber, number)
    message("Downloaded number: ", i,"| Still remaining: ", result$pocetZostavajucichId)
    number <- result$id[length(result$id)]
    completeList[[as.character(i)]] <- data.frame(result$id)
    if (result$pocetZostavajucichId <= 0) {
      break
    }
    i <- i + 1
  }
  res <- completeList %>% dplyr::bind_rows()
  res
}
