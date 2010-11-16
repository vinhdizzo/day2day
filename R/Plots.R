##' Save the plot to pdf, eps, jpeg, and tikz (tex).
##'
##' See \code{\link[grDevices]{dev2}}.  The \code{\link[agsemisc]{plotf}} function also saves to multiple formats.
##' @title Save plot to multiple files
##' @param file Filename without the extension.
##' @param plot.expression Plotting expression.  If missing, copy the currently active device.
##' @param height Height in inches.
##' @param width Width in inches.
##' @param keep.active If \code{TRUE}, then keep the original plotting device open.
##' @return \code{\link{invisible}}.
##' @author Vinh Nguyen
##' @examples
##' ## Ex 1
##' SavePlot(file="Rplots", plot.expression=plot(1:5))
##' ## Ex 2
##' plot(1:5)
##' SavePlot(file="Rplots")
##' @import tikzDevice
SavePlot <- function(file="Rplot%03d", plot.expression, height=7, width=7, keep.active=FALSE) {
  require(tikzDevice)
  filenames <- paste(file, c("pdf", "eps", "jpeg", "tex"), sep=".")
  ##tikz(file=filenames[4], height=height, width=width)
  if(!missing(plot.expression)) {
    eval(plot.expression)
  }
  ##dev.copy2pdf(file=filenames[1], height=height, width=width)
  ##dev.copy2eps(file=filenames[2], height=height, width=width)
  dev.copy(device=pdf, file=filenames[1], height=height, width=width)
  dev.off()
  setEPS()
  dev.copy(device=postscript, file=filenames[2], height=height, width=width)
  dev.off()
  dev.copy(device=jpeg, file=filenames[3], height=height, width=width, units="in", res=300)
  dev.off()
  dev.copy(device=tikz, file=filenames[4], height=height, width=width)
  dev.off()
  if(!keep.active) dev.off()
  return(invisible(NULL))
}
