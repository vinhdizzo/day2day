##' Save the plot to pdf, eps, jpeg, and tikz (tex).
##'
##' See \code{\link[grDevices]{dev.copy}}.  The \code{\link[agsemisc]{plotf}} function also saves to multiple formats.
##' @title Save plot to multiple files
##' @param file Filename without the extension.  "Rplot\%03d" for multiple plots will probably not work based on how this is written.
##' @param plot.expression Plotting expression.  If missing, copy the currently active device.
##' @param height Height in inches.
##' @param width Width in inches.
##' @param keep.active If \code{TRUE}, then keep the original plotting device open.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' ## Ex 1
##' \dontrun{SavePlot(file="Rplots", plot.expression=plot(1:5))}
##' ## Ex 2
##' \dontrun{plot(1:5)}
##' \dontrun{SavePlot(file="Rplots")}
##' @import tikzDevice
##' @export
SavePlot <- function(file="Rplot", plot.expression, height=7, width=7, keep.active=FALSE) {
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
