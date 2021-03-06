% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endpoints.R
\name{getChangedFinancialStatements}
\alias{getChangedFinancialStatements}
\title{Get all changed financial statements}
\usage{
getChangedFinancialStatements(
  from = "2019-01-01",
  maxNumber = 10000,
  afterId = 1
)
}
\arguments{
\item{from}{\code{character} with date since when the changes happened.}

\item{maxNumber}{\code{numeric} with the number of changed records that should be pulled.}

\item{afterId}{\code{numeric} after which ID should we get changed records.}
}
\value{
with changed records.
}
\description{
Get all changed financial statements.
}
