#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
toCharDF <- function(x) {
  x[] <- lapply(x, as.character)
  x
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
addDayToDate <- function(x, day = "01") {
  paste(x, day, sep = "-")
}
