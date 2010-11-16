\name{SavePlot}
\alias{SavePlot}
\title{Save plot to multiple files}
\usage{SavePlot(file="Rplot", plot.expression, height=7, width=7, keep.active=FALSE)
}
\description{Save the plot to pdf, eps, jpeg, and tikz (tex).}
\details{See \code{\link[grDevices]{dev2}}.  The \code{\link[agsemisc]{plotf}} function also saves to multiple formats.}
\value{Nothing.}
\author{Vinh Nguyen}
\arguments{\item{file}{Filename without the extension.  "Rplot\%03d" for multiple plots will probably not work based on how this is written.}
\item{plot.expression}{Plotting expression.  If missing, copy the currently active device.}
\item{height}{Height in inches.}
\item{width}{Width in inches.}
\item{keep.active}{If \code{TRUE}, then keep the original plotting device open.}
}
\examples{## Ex 1
\dontrun{SavePlot(file="Rplots", plot.expression=plot(1:5))}
## Ex 2
\dontrun{plot(1:5)}
\dontrun{SavePlot(file="Rplots")}}

