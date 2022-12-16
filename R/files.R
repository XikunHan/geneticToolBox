





#' list all files with detailed information in a folder
#'
#' List all files with detailed information in a folder. This function is very useful to check large files, eg. GWAS summary statistics.
#' @param path Full path to list.
#' @param pattern Match patter. See \code{\link{list.files}}.
#' @param recursive Recursive into sub-folders.
#' @param full.names List full name.
#' @param ignore.case Case-insensitive.
#' @param all.files Whether to show invisible files.
#' @export
#'
file_info <- function(path = ".", pattern = NULL,  recursive = TRUE, full.names = TRUE, ignore.case = FALSE, all.files = FALSE) {
  x <- list.files(path =  path, pattern = pattern, recursive = recursive, full.names = full.names, ignore.case = ignore.case, all.files = all.files)

  v_n <- length(x)

  cat(paste0("Number of files: ", v_n, "\n"))

  df_one <- cbind(file = "", data.table(file.info("")))
  df_info_all <- df_one[1:v_n, ]


  pb <- txtProgressBar(min = 1, max = v_n, style = 3, file = stderr())

  for(i in 1L:v_n) {
    setTxtProgressBar(pb = pb, value = i)
    v_i <- x[i]

    df_one <- cbind(file = v_i, data.table(file.info(v_i)))
    set(df_info_all, i = i,  names(df_info_all), value =  df_one)
  }

  close(con = pb)
  df_info_all$size_MB <- df_info_all$size/1024/1024
  df_info_all$file_name <- basename(df_info_all$file)
  return(df_info_all)
}





#' read the last line of all files in a folder
#'
#' Read the last line of all files in a folder. This function is useful to check log files.
#' @param path Full path to list.
#' @param pattern Match patter. See \code{\link{list.files}}.
#' @param recursive Recursive into sub-folders.
#' @param full.names List full name.
#' @param ignore.case Case-insensitive.
#' @param all.files Whether to show invisible files.
#' @export
#'
#'
read_last_line <- function(path = ".", pattern = NULL,  recursive = TRUE, full.names = TRUE, ignore.case = FALSE, all.files = FALSE)  {

  x <- list.files(path =  path, pattern = pattern, recursive = recursive, full.names = full.names, ignore.case = ignore.case, all.files = all.files)

  v_n <- length(x)

  cat(paste0("Number of files: ", v_n, "\n"))

  df<- data.table(file = "", content = "")
  df <- df[1:v_n, ]

  df$file <- x

  lastline <- function(filename) {
    n <- system(paste0("wc -l ",filename, " | awk '{print $1}' "),intern=TRUE)
    scan(filename,what="",skip=as.integer(n) -1 , nlines=1,sep="\n",quiet=TRUE)
  }

  pb <- txtProgressBar(min = 1, max = v_n, style = 3, file = stderr())
  for(i in 1L:v_n) {
    setTxtProgressBar(pb = pb, value = i)
    v_i <- x[i]
    v_content <- lastline(v_i)

    if(length(v_content) == 0) {
      set(df, i = i,  "content", value = "")
    } else {
      set(df, i = i,  "content", value = v_content)
    }
  }
  df$file_name <- basename(df$file)
  return(df)
}




#' read in a batch of files
#'
#' read in a batch of files in the same format
#' @param path Full path to file.
#' @param pattern Match patter. See \code{\link{list.files}}.
#' @param recursive Default false. See \code{\link{list.files}}.
#' @export
#'
#'
load_files <- function (path = ".", pattern = NULL, recursive = FALSE) 
{
  v_files <- list.files(path = path, pattern = pattern, recursive = recursive)
  print(paste0("file N = ", length(v_files)))
  df_res <- NULL
  pb <- txtProgressBar(min = 0, max = length(v_files), style = 3, 
                       file = stderr())
  for (j in 1L:length(v_files)) {
    setTxtProgressBar(pb = pb, value = j)
    df_one <- data.table::fread(file.path(path, v_files[j]), 
                                fill = TRUE)
    df_res <- rbind(df_res, cbind(df_one, file = v_files[j]), fill = TRUE)
  }
  return(df_res)
}

