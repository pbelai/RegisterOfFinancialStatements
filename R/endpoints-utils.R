#' Return list object from URL
#'
#' Read lines from the URL and returns results as `list`.
#'
#' @param url `character` from where the data should be read.
#'
#' @return `list` with the read data.
getObjectFromURL <- function(url) {
  jsonlite::fromJSON(readLines(url, warn = FALSE, encoding = "UTF-8"))
}

#' Create query
#'
#' Creates query from the endpoints and parameters.
#'
#' @param endpoint `character` endpoint.
#' @param ... params that should be sent to endpoint
#' @param baseUrl `character` base url of the API.
#'
#' @return `character` url.
createUrl <- function(endpoint, ..., baseUrl = "http://www.registeruz.sk/cruz-public/api") {
  params <- list(...)
  params <- paste(names(params), params, sep = "=")
  if (length(params) == 0) {
    paste0(baseUrl, endpoint)
  } else {
    paste0(baseUrl, endpoint, "?", paste0(unlist(params), collapse = "&"))
  }
}

#' Query URL multiple times
#'
#' Queries URL 'numberOfTries' until success or warning for url is logged.
#'
#' @param url `character` url to query.
#' @param numberOfTries `numeric` number of tries that should be done.
#' @param fun `function` used for quering.
#'
#' @return `object`
#' @examples
tryUntilSuccess <- function(url, numberOfTries = 20, fun = rvest::html) {
  if (numberOfTries == 0) {
    warning("Unable to read from: ", url)
    NULL
  } else {
    res <- tryCatch(
      fun(url),
      error = function(x) {tryUntilSuccess(url, numberOfTries - 1, fun)}
    )
    res
  }
}
