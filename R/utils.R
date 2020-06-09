#' Transform all columns to character
#'
#' Transform all columns to character.
#'
#' @param x `data.frame`
#'
#' @return `chracater` `data.frame`.
toCharDF <- function(x) {
  x[] <- lapply(x, as.character)
  x
}

#' Add day to date string
#'
#' Add day to date string
#'
#' @param x `character` date.
#' @param day `character` to append.
#'
#' @return `character` with day added.
addDayToDate <- function(x, day = "01") {
  paste(x, day, sep = "-")
}
