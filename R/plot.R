#' ggsave in different formats
#'
#' ggsave in different formats, including pdf, tiff, eps, and png (different journals may require different formats).
#' @param filename Filename
#' @param \dots Further arguments to pass.
#' @export
#'
ggsave_all <- function(filename, ...) {
  ggplot2::ggsave(paste0(filename,".pdf"), ...)
  ggplot2::ggsave(paste0(filename,".tiff"), ...)
  ggplot2::ggsave(paste0(filename,".eps"), ...)
  ggplot2::ggsave(paste0(filename,".png"), ...)
  return(1)
}


#' save_plot in different formats
#'
#' save_plot in different formats, including pdf, tiff, eps, and png (different journals may require different formats).
#' @param filename Filename
#' @param \dots Further arguments to pass.
#' @export
#'
save_plot_all <- function(filename, ...) {
  cowplot::save_plot(paste0(filename,".pdf"), ...)
  cowplot::save_plot(paste0(filename,".tiff"), ...)
  cowplot::save_plot(paste0(filename,".eps"), ...)
  cowplot::save_plot(paste0(filename,".png"), ...)
  return(1)
}
