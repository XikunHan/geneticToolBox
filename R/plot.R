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





#' venn plot
#'
#' A wrapper function of venn plot.
#' save_plot in different formats, including pdf, tiff, eps, and png (different journals may require different formats).
#' @param x A list of data.
#' @param color color for the venn, default is Set1 from brewer.pal.
#' @param category.names label, default is the names in the list.
#' @param filename file name to save
#' @param imagetype "png" or "tiff"
#' @param height default value 800
#' @param width default value 1200
#' @param units default value "px",
#' @param resolution default value 300
#' @param compression default value "lzw"
#' @param lwd default value 3,
#' @param lty default value 'blank',
#' @param fill default value is the same as color
#' @param cex default value .8,
#' @param fontfamily default value "sans",
#' @param cat.cex default value 0.8,
#' @param cat.default.pos default value "outer",
#' @param cat.pos default value c(0, 0, 0, 0, 0, 0, 0),
#' @param cat.dist default value c(0.1, 0.1, 0.1, 0.1, 0.1,0.1, 0.1)
#' @export
#'



plot_venn <- function(x, color = NULL, category.names = NULL,
                      filename = NULL,
                      imagetype = "png", height = 800, width = 1200, units = "px",
                      resolution = 300, compression = "lzw",   lwd = 3,
                      lty = 'blank',
                      fill = color,
                      # Numbers
                      cex = .8,
                      fontfamily = "sans",
                      cat.cex = 0.8,
                      cat.default.pos = "outer",
                      cat.pos = c(0, 0, 0, 0, 0, 0, 0),
                      cat.dist = c(0.1, 0.1, 0.1, 0.1, 0.1,0.1, 0.1)) {

  if(is.null(color)) {
    color <- RColorBrewer::brewer.pal(9, "Set1")[1:length(x)]
  }


if(is.null(filename)) {
  dir.create("~/todelete", showWarnings = FALSE)
  filename <- '~/todelete/test.png'
}

  if(is.null(category.names)) {
    category.names <- names(x)
  }

  cat.pos <- cat.pos[1:length(x)]
  cat.dist <-cat.dist[1:length(x)]

 p <- VennDiagram::venn.diagram(
  x = x,
  category.names = category.names,
  filename = NULL,
  output = output,


  imagetype = imagetype ,
  height = height,
  width = width ,
  resolution = resolution,
  compression = compression,

  # Circles
  lwd = lwd,
  lty = lty,
  fill = color,

  # Numbers
  cex = cex,
  fontfamily = fontfamily,

  cat.cex = cat.cex,
  cat.default.pos = cat.default.pos,
  cat.pos = cat.pos,
  cat.dist = cat.dist,
  cat.col = color
)

 if ("tiff" == imagetype) {
   tiff(filename = filename, height = height, width = width,
        units = units, res = resolution, compression = compression)
 } else if ("png" == imagetype) {
   png(filename = filename, height = height, width = width,
       units = units, res = resolution)
 }
 grid::grid.draw(p)
 dev.off()
 dev.off()
 return(grid::grid.draw(p))
}



