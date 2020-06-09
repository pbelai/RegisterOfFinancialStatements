#' Get base of financial report
#'
#' Get base of financial report.
#'
#' @param finReport financial report `list`.
#'
#' @return `data.frame` with base of financial report.
getBaseFinReport <- function(finReport) {
  finReport[!names(finReport) %in% c("prilohy", "obsah", "idUctovnejZavierky", "id")] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    cbind(id_financial_report = finReport$id)
}


#' Get title page of financial report
#'
#' Get title page of financial report.
#'
#' @param finReport financial report `list`.
#'
#' @return `data.frame` with title page of financial report.
getTitleFinReport <- function(finReport) {
  .normalizeDate <- function(x) {
    if (!is.null(x) && !is.na(x)) addDayToDate(x) else x
  }

  ncol(getAktivaFinReport(finReport))

  title <- finReport$obsah$titulnaStrana
  title$adresa <- paste(title$adresa, collapse = " ")
  if (ncol(getAktivaFinReport(finReport)) == 47) {
    title$type <- "MUJ"
  } else if (ncol(getAktivaFinReport(finReport)) == 313) {
    title$type <- "POD"
  }

  title <- title %>% as.data.frame(stringsAsFactors = FALSE)
  title$obdobieOd <- .normalizeDate(title$obdobieOd)
  title$obdobieDo <- .normalizeDate(title$obdobieDo)
  title$predchadzajuceObdobieDo <- .normalizeDate(title$predchadzajuceObdobieDo)
  title$predchadzajuceObdobieOd <- .normalizeDate(title$predchadzajuceObdobieOd)

  cbind(id_financial_report = finReport$id, title)
}

#' Get data from financial report on position
#'
#' Get data from financial report on position.
#'
#' @param finReport financial report `list`.
#' @param position `numeric` position of data which should be selected.
#'
#' @return `data.frame` with the subsetted data.
getNumericReportData <- function(finReport, position) {
  finReport$obsah$tabulky[[position]]$data %>%
    as.numeric() %>%
    t() %>%
    as.data.frame() %>%
    cbind(id_financial_report = finReport$id)
}



#' @describeIn getNumericReportData
#' Get assets data from financial report.
getAktivaFinReport <- function(finReport) {
  getNumericReportData(finReport, 1)
}


#' @describeIn getNumericReportData
#' Get liabilities and equity data from financial report.
getPasivaFinReport <- function(finReport) {
  getNumericReportData(finReport, 2)
}


#' @describeIn getNumericReportData
#' Get income statement data from financial report.
getZSFinReport <- function(finReport) {
  getNumericReportData(finReport, 3)
}
