



#' check data sets before merging
#'
#' Check the class, duplication, and missingness of ID columns. A Venn plot is provided.
#' @param x,y data tables.
#' @param by  Shared column name in x and y to merge on.
#' @param by.x,by.y Column names in x and y to merge on.
#' @export
#'
merge_check <- function(x, y, by = NULL, by.x = NULL, by.y = NULL) {

  x <- as.data.table(x)
  y <- as.data.table(y)

  if(!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  id_x <- x[,get(by.x)]
  id_y <- y[,get(by.y)]

  cat(paste0("X: ",class(id_x), ";  duplicate values: ", sum(duplicated(id_x), na.rm = TRUE), ";  missing values: ", sum(is.na(id_x)), ". \n"))
  cat(paste0("Y: ",class(id_y), ";  duplicate values: ", sum(duplicated(id_y), na.rm = TRUE), ";  missing values: ", sum(is.na(id_y)), ". \n"))

  v_list <- list(x = unique(id_x),
                 y = unique(id_y))

  gplots::venn(v_list)
}



#' merge multiple data tables
#'
#' @param data_list A list of data tables.
#' @param by Shared column name to merge on.
#' @param all Whether extra rows will be added to the output.
#' @param sort Whether the result is sorted to the 'by' column.
#' @export
#'
mergeDataSets <- function(data_list, by = NULL, all = TRUE, sort = FALSE) {
  Reduce( function(...) merge(..., by = by, all = all, sort = sort) , data_list)
}





#' meta-analysis based on inverse variance method.
#'
#' @param beta1 effect size for the first study.
#' @param beta2 effect size for the second study.
#' @param se1 standard error for the first study.
#' @param se2 standard error for the second study.
#' @export
meta_inverse_variance <- function(beta1, beta2, se1, se2) {
  if(is.na(beta1)) {
    b <- beta2
    se <- se2
    z = b/se
    p = pnorm(-1* abs(z))*2
    return(list(b = b, se = se, p = p))
  } else if(is.na(beta2)) {
    b <- beta1
    se <- se1
    z = b/se
    p = pnorm(-1* abs(z))*2
    return(list(b = b, se = se, p = p))
  } else {
    w1 = 1/se1^2
    w2 = 1/se2^2
    se = sqrt(1/(w1+w2))
    b = (beta1*w1 + beta2*w2)/(w1 + w2)
    z = b/se
    p = pnorm(-1* abs(z))*2
    return(list(b = b, se = se, p = p))
  }
}


#' @description meta-analysis based on inverse variance method for a data.
#' @param data if a data.table, the beta and se are the column names.
#' @rdname meta_inverse_variance
#' @export
meta_inverse_variance_df <- function(data = NULL, beta1, beta2, se1, se2) {
  df_res <- as.data.table(data)
  df_res$b_meta <- NA_real_
  df_res$se_meta <- NA_real_
  df_res$p_meta <-  NA_real_

  v_n <- NROW(df_res)
  pb <- txtProgressBar(min = 1, max = v_n, style = 3, file = stderr())

  for(i in 1L:v_n) {
    setTxtProgressBar(pb = pb, value = i)
    df_one <- meta_inverse_variance(df_res[i, get(beta1)], df_res[i, get(beta2)], df_res[i, get(se1)], df_res[i, get(se2)])
    # value should be a list for multiple columns.
    df_res[i, c("b_meta", "se_meta", "p_meta") := list(df_one$b, df_one$se, df_one$p)]
  }
  close(con = pb)
  return(df_res)
}
