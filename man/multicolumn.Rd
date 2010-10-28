\name{multicolumn}
\alias{multicolumn}
\title{LaTeX: multicolumn}
\usage{multicolumn(text, ncol, align="c")
}
\description{Generate a \code{multicolumn} statement for used with \code{paste} and \code{cat} in table LaTeX table generation.}
\value{Character value of multicolumn statement.}
\author{Vinh Nguyen}
\arguments{\item{text}{Text to be displayed in table cell.}
\item{ncol}{Number of columns to span.}
\item{align}{Alignment, e.g., "c" for centered.}
}
\examples{multicolumn("Hello world!", ncol=3)}

