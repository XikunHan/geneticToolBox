% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse.R
\name{mergeDataSets}
\alias{mergeDataSets}
\title{merge multiple data tables}
\usage{
mergeDataSets(data_list, by = NULL, all = TRUE, sort = FALSE)
}
\arguments{
\item{data_list}{A list of data tables.}

\item{by}{Shared column name to merge on.}

\item{all}{Whether extra rows will be added to the output.}

\item{sort}{Whether the result is sorted to the 'by' column.}
}
\description{
merge multiple data tables
}
