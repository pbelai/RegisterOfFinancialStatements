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
