#' Append new accounting entity data
#'
#' @param accEntity
#'
#' @return
#' @export
#'
#' @examples
appendAccountingEntity <- function(accEntity) {
  con <- getDBConnection()
  accEntity <- as.data.frame(accEntity, stringsAsFactors = FALSE) %>%
    dplyr::rename("id_accounting_entity" = "id") %>%
    setNames(tolower(names(.)))


  financialStatementsIDs <- as.character(accEntity$iductovnychzavierok)
  accEntity$id_accounting_entity <- as.character(accEntity$id_accounting_entity)

  finStatementsForAccEntitiy <- data.frame(
      id_accounting_entity = accEntity$id_accounting_entity,
      id_financial_statement = financialStatementsIDs,
      stringsAsFactors = FALSE
    )

  accEntity <-  accEntity[,colnames(accEntity) != "iductovnychzavierok"] %>% unique()

  RPostgres::dbBegin(con)
  appendIfMissing(accEntity, "accounting_entity", con)
  appendIfMissing(finStatementsForAccEntitiy, "financial_statement_for_accounting_entity", con)
  RPostgres::dbCommit(con)
}


#' Append new financial statement data
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
appendFinancialStatement <- function(finStatement) {
  con <- getDBConnection()
  finStatement <- as.data.frame(finStatement, stringsAsFactors = FALSE) %>%
    dplyr::rename("id_financial_statement" = "id") %>%
    setNames(tolower(names(.)))

  financialReportsIDs <- finStatement$iductovnychvykazov

  finReportsForFinStatement <- data.frame(
    id_financial_report = financialReportsIDs,
    id_financial_statement = finStatement$id_financial_statement,
    stringsAsFactors = FALSE
  )

  finStatementForAccEntity <- finStatement %>%
    dplyr::rename("accounting_entity" = "iduj") %>%
    .[c("accounting_entity", "id_financial_statement")] %>%
    toCharDF()

  finStatement <-  finStatement[,colnames(finStatement) != "iductovnychvykazov"] %>% unique()
  finStatement$obdobieod <- addDayToDate(finStatement$obdobieod)
  finStatement$obdobiedo <- addDayToDate(finStatement$obdobiedo)

  RPostgres::dbBegin(con)
  appendIfMissing(finStatement, "financial_statement", con)
  appendIfMissing(finReportsForFinStatement, "financial_report_for_financial_statement", con)
  appendIfMissing(finStatementForAccEntity, "financial_statement_for_accounting_entity", con)
  RPostgres::dbCommit(con)
}


#' Title
#'
#' @param finReport
#'
#' @return
#' @export
#'
#' @examples
appendFinancialReport <- function(finReport) {
  .appendType <-
    function(x, finReportTitle) {
      if (finReportTitle$type == "MUJ")
        paste0(x, "_muj")
      else
        paste0(x, "_pod")
    }
  con <- getDBConnection()
  finReportBase <- getBaseFinReport(finReport)
  finReportTitle <- getTitleFinReport(finReport)
  finReportsForFinStatement <- data.frame(
    id_financial_report = finReport$id,
    id_financial_statement = finReport$idUctovnejZavierky,
    stringsAsFactors = FALSE
  )

  RPostgres::dbBegin(con)
  appendIfMissing(finReportBase, "financial_report_base", con)
  appendIfMissing(finReportTitle, "financial_report_title", con)
  appendIfMissing(finReportsForFinStatement, "financial_report_for_financial_statement", con)
  appendIfMissing(getAktivaFinReport(finReport), .appendType("financial_report_assets", finReportTitle), con)
  appendIfMissing(getPasivaFinReport(finReport), .appendType("financial_report_lae", finReportTitle), con)
  appendIfMissing(getZSFinReport(finReport), .appendType("financial_report_is", finReportTitle), con)
  RPostgres::dbCommit(con)
}



