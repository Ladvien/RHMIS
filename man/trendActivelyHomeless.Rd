% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HomelessTrend.R
\name{trendActivelyHomeless}
\alias{trendActivelyHomeless}
\title{Merges two HMIS CSV sets.}
\usage{
trendActivelyHomeless(allDataPath, dropboxPath, interval = "week",
  startDate = "", servicesThreshold = 0)
}
\arguments{
\item{string}{path to folder contaning first CSV set.}

\item{string}{path to folder contaning second CSV set.}

\item{string}{path to output where merged CSVs will be written.}

\item{boolean}{save in feather file format.  Default is false.}
}
\description{
Merges two HMIS CSV sets.
}
\examples{

mergeHmisCSVs('/Users/user/local/CSV_2016', 
              '/Users/user/local/CSV_2016',
              '/Users/user/local/CSV_2016_and_2017')
mergeHmisCSVs('/Users/user/local/CSV_2016', 
              '/Users/user/local/CSV_2016',
              '/Users/user/local/CSV_2016_and_2017'
               feather = TRUE)
}
\seealso{
\code{\link{nchar}} which this function wraps
}
