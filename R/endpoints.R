getChangedAccountingEntities <- function(from = "2019-01-01") {
  createUrl("/uctovne-jednotky", "zmenene-od" = from, "max-zaznamov" = 10000, "pokracovat-za-id" = 1) %>% tryUntilSuccess(getObjectFromURL)
}

getChangedFinancialStatements <- function(from = "2019-01-01") {
  createUrl("/uctovne-zavierky", "zmenene-od" = from, "max-zaznamov" = 10000, "pokracovat-za-id" = 1) %>% tryUntilSuccess(getObjectFromURL)
}

getChangedFinancialReports <- function(from = "2019-01-01") {
  createUrl("/uctovne-vykazy", "zmenene-od" = from, "max-zaznamov" = 10000, "pokracovat-za-id" = 1) %>% tryUntilSuccess(getObjectFromURL)
}

getAccountingEntityDetails <- function(id) {
  createUrl("/uctovna-jednotka", id = id) %>% tryUntilSuccess(getObjectFromURL)
}

getFinancialStatementDetails <- function(id) {
  createUrl("/uctovna-zavierka", id = id) %>% tryUntilSuccess(getObjectFromURL)
}

getFinancialStatementDetails <- function(id) {
  createUrl("/uctovny-vykaz", id = id) %>% tryUntilSuccess(getObjectFromURL)
}
