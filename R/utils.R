toCharDF <- function(x) {
  x %>%
    apply(2, as.character) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

addDayToDate <- function(x) {
  paste0(x, "-01")
}
