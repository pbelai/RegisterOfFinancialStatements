downloadEntitiesWithStattements <- function(from = "2020-01-01") {
  changedAccountingEntities <- getAllChangedAccountingEntities(from = from)
  saveRDS(changedAccountingEntities, "changedAccountingEntities.rds")

  changedFinancialReports <- getAllChangedFinancialReports(from = from)
  saveRDS(changedFinancialReports, "changedFinancialReports.rds")

  batchSuffix <- "-newBatch"

  downloadBatchDetails(changedFinancialReports, getFinancialReportDetails, batch = 1000, dir = paste0("finStatements", batchSuffix))

  getStatementsData("MUJ", paste0("finStatements", batchSuffix)) %>%
    saveRDS(paste0("finStatementMUJ", batchSuffix, ".rds"))
  getStatementsData("POD", paste0("finStatements", batchSuffix)) %>%
    saveRDS(paste0("finStatementPOD", batchSuffix, ".rds"))

  splitDataByType("MUJ",batchSuffix)
  splitDataByType("POD",batchSuffix)

  writeCSVfiles("MUJ",batchSuffix)
  writeCSVfiles("POD",batchSuffix)
}

downloadBatchDetails <- function(ids, fun, batch = 1000, dir = "accEntities") {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  idsChunks <- split(ids, ceiling(seq_along(ids) / batch))
  pos <- 1
  lapply(idsChunks, function(chunk) {
    message("Downloading chunk ", pos)
    lapply(chunk, function(id) {
      cat("\r Current id: ",id)
      fun(id)
    }) %>% saveRDS(paste0(dir, "/", pos, ".rds"))
    cat("\n")
    pos <<- pos + 1
  })
}

getStatementsData <- function(type, dirPath = "finStatements") {
  list.files(dirPath) %>% lapply(function(rdsFile) {
    cat("\r Current file: ",rdsFile)
    readRDS(file.path(dirPath, rdsFile)) %>% lapply(function(finStatement) {
      if (!is.null(finStatement$obsah$titulnaStrana)) {
        if (!is.null(finStatement$obsah$titulnaStrana$obdobieDo) &&
            (startsWith(finStatement$obsah$titulnaStrana$obdobieDo, "2019") ||
            startsWith(finStatement$obsah$titulnaStrana$obdobieOd, "2019")
            )
            ) {
          titulna <- getTitleFinReport(finStatement)
          if (!is.null(titulna$type) && titulna$type == type) {
            list(titulna = titulna,
                 aktiva = getAktivaFinReport(finStatement),
                 pasiva = getPasivaFinReport(finStatement),
                 zs = getZSFinReport(finStatement))
          }
        }
      }
    }) %>% Filter(function(x) {!is.null(x)},.)
  })
}

extractData <- function(finStatementsData, extract) {
  finStatementsData %>% lapply(function(x){
    x %>% lapply(function(y){
      y[[extract]]
    }) %>% data.table::rbindlist(fill = TRUE)
  }) %>% data.table::rbindlist(fill = TRUE)
}

splitDataByType <- function(type, suffix = NULL) {
  sheets <- c("titulna", "aktiva", "pasiva", "zs")
  print(paste0("finStatement",type,suffix,".rds"))
  finStatementsData <- readRDS(paste0("finStatement",type,suffix,".rds"))
  if (!dir.exists((type))) {
    dir.create(type)
  }
  lapply(sheets, function(sheet) {
    finStatementsData %>%
      extractData(sheet) %>%
      saveRDS(file.path(type, paste0(sheet, suffix, ".rds")))
  })

}

writeCSVfiles <- function(type, suffix = NULL) {
  .getColNames <- function(numberOfCols, type, fileType) {
    if (type == "POD" && fileType == "aktiva") {
      c(paste0("s",rep(1:4, numberOfCols / 4), "r", rep(1:(numberOfCols / 4), each = 4)),"id_financial_report")
    } else {
      c(paste0("s",rep(1:2, numberOfCols / 2), "r", rep(1:(numberOfCols / 2), each = 2)),"id_financial_report")
    }
  }

  .reorder <- function(x) {
    colNames <- colnames(x)
    colNames <- colNames[colNames != "id_financial_report"]
    column <- colNames %>% gsub("s(.*)r(.*)", "\\1",.) %>% as.numeric()
    row <- colNames %>% gsub("s(.*)r(.*)", "\\2",.) %>% as.numeric()
    x[c("id_financial_report",colNames[order(column, row)])]
  }

  .writeNumericType <- function(fileType, titulne) {
    readRDS(file.path(type, paste0(fileType,suffix,".rds"))) %>%
      setNames(., .getColNames(ncol(.), type, fileType)) %>%
      .reorder() %>%
      dplyr::inner_join(titulne, ., by = "id_financial_report") %>%
      write.csv(file = file.path(type, paste0(fileType,suffix,".csv")), fileEncoding = "UTF-8", row.names = FALSE, na = "")
  }

  titulne <- readRDS(paste0(type, "/titulna",suffix,".rds"))
  titulne %>% write.csv(file = file.path(type, paste0("titulne",suffix,".csv")), fileEncoding = "UTF-8", row.names = FALSE, na = "")
  .writeNumericType("aktiva", titulne)
  .writeNumericType("pasiva", titulne)
  .writeNumericType("zs", titulne)
}
