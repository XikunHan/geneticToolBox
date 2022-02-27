#' @import data.table
#' @import ggplot2
#' @importFrom gplots venn
#' @importFrom stats pnorm
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
NULL





#' cross tabulation showing NA
#' @param \dots Arguments to pass.
#' @export
#'
tab <- function(...) table(..., useNA = "ifany")


#' format digits
#'
#' Keep two digits for numeric values.
#' @param x A value or vector.
#' @export
#'
format_digit <- function(x) {
  x <- ifelse(abs(x)<0.01,sprintf(fmt="%.2E",x),sprintf(fmt="%.2f",x))
  return(x)
}


