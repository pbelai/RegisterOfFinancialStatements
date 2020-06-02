getChangedAccountingEntities <- function(from = "2019-01-01") {
  createUrl("/uctovne-jednotky", "zmenene-od" = from, "max-zaznamov" = 10000, "pokracovat-za-id" = 1)
}

getAccountingEntities <- function() {
  createUrl("/uctovne-jednotky", "zmenene-od" = "2020-01-01", "max-zaznamov" = 10000, "pokracovat-za-id" = 1)
}
