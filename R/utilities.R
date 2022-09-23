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


#' create essential project folder 
#' @param name project name
#' @param path directory of the project
#' @export
#'
create_project <- function(name = NULL, path = NULL)  {
  
  # Ex: create_project(name = "target", path = "/Users/xih628/Documents/project")
  
  if(is.null(path)) {
    path <- getwd()
    cat(paste0("\nWorking in the current folder: "), path)
  }
  
  if(is.null(name)) {
    name <-  basename(path)
    cat(paste0("\nProject name is exist: "), name)
  } else {
    path <- file.path(path, name)
  }
  
  cat(paste0("\nProject at: "), path)
  
  
  for (folder in c("data", "script", "output", "result", "result/table", "result/figure")) {
    dir.create(file.path(path, folder), showWarnings = FALSE)
  }
  
}
