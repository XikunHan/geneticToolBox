% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse.R
\name{merge_check}
\alias{merge_check}
\title{check data sets before merging}
\usage{
merge_check(x, y, by = NULL, by.x = NULL, by.y = NULL)
}
\arguments{
\item{x, y}{data tables.}

\item{by}{Shared column name in x and y to merge on.}

\item{by.x, by.y}{Column names in x and y to merge on.}
}
\description{
Check the class, duplication, and missingness of ID columns. A Venn plot is provided.
}
