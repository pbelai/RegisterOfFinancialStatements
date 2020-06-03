getChangedAccountingEntities <- function(from = "2019-01-01") {
  createUrl("/uctovne-jednotky", "zmenene-od" = from, "max-zaznamov" = 10000, "pokracovat-za-id" = 1) %>% getObjectFromURL()
}

getAccountingEntities <- function() {
  createUrl("/uctovne-jednotky", "zmenene-od" = "2020-01-01", "max-zaznamov" = 10000, "pokracovat-za-id" = 1) %>% getObjectFromURL()
}

getAccountingEntityDetails <- function(id) {
  createUrl("/uctovna-jednotka", id = id) %>% getObjectFromURL()
}

getFinancialStatementDetails <- function(id) {
  createUrl("/uctovna-zavierka", id = id) %>% getObjectFromURL()
}
