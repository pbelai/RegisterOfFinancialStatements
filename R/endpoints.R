getChangedAccountingEntities <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-jednotky", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = afterId) %>%
    tryUntilSuccess()
}

getChangedFinancialStatements <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-zavierky", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = afterId) %>%
    tryUntilSuccess()
}

getChangedFinancialReports <- function(from = "2019-01-01", maxNumber = 10000, afterId = 1) {
  createUrl("/uctovne-vykazy", "zmenene-od" = from, "max-zaznamov" = maxNumber, "pokracovat-za-id" = afterId) %>%
    tryUntilSuccess()
}

getAccountingEntityDetails <- function(id) {
  createUrl("/uctovna-jednotka", id = id) %>%
    tryUntilSuccess()
}

getFinancialStatementDetails <- function(id) {
  createUrl("/uctovna-zavierka", id = id) %>%
    tryUntilSuccess()
}

getFinancialReportDetails <- function(id) {
  createUrl("/uctovny-vykaz", id = id) %>%
    tryUntilSuccess()
}
