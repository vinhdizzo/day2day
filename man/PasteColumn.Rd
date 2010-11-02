\name{PasteColumn}
\alias{PasteColumn}
\title{Paste columns of a matrix or data frame}
\usage{PasteColumn(x, sep=" ")
}
\description{Paste columns of a matrix or data frame together.  See \code{\link{paste}}.}
\value{A vector of strings.}
\author{Vinh Nguyen}
\arguments{\item{x}{A matrix or data frame.}
\item{sep}{The delimiter to concatenate the columns of \code{x} together.}
}
\examples{PasteColumn(cbind(rep(1:10), rep(1:10)))}

