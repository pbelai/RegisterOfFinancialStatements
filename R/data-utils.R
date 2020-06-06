#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
getBaseFinReport <- function(finReport) {
  finReport[!names(finReport) %in% c("prilohy", "obsah")] %>% as.data.frame(stringsAsFactors = FALSE)
}


#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
getTitleFinReport <- function(finReport) {
  .normalizeDate <- function(x) {
    if (!is.null(x) && !is.na(x)) addDayToDate(x) else x
  }
  title <- finReport$obsah$titulnaStrana
  title$adresa <- paste(title$adresa, collapse = " ")
  if (is.null(title$typUctovnejJednotky)) {
    title$type <- "MUJ"
  } else {
    title$type <- "POD"
  }

  title <- title %>% as.data.frame(stringsAsFactors = FALSE)
  title$obdobieOd <- .normalizeDate(title$obdobieOd)
  title$obdobieDo <- .normalizeDate(title$obdobieDo)
  title$predchadzajuceObdobieDo <- .normalizeDate(title$predchadzajuceObdobieDo)
  title$predchadzajuceObdobieOd <- .normalizeDate(title$predchadzajuceObdobieOd)

  cbind(id_financial_report = finReport$id, title)
}


#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
getAktivaFinReport <- function(finReport) {
  getNumericReportData(finReport, 1)
}


#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
getPasivaFinReport <- function(finReport) {
  getNumericReportData(finReport, 2)
}


#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
getZSFinReport <- function(finReport) {
  getNumericReportData(finReport, 3)
}

#' Title
#'
#' @param finReport
#' @param position
#'
#' @return
#' @export
#'
#' @examples
getNumericReportData <- function(finReport, position) {
  finReport$obsah$tabulky[[position]]$data %>%
    as.numeric() %>%
    t() %>%
    as.data.frame() %>%
    cbind(id_financial_report = finReport$id)
}

