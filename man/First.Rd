\name{First}
\alias{First}
\title{First}
\usage{First(x, index=FALSE)
}
\description{For input \code{x} where the values are grouped, determine where the first element of each group occurs.}
\details{This function is useful in longitudinal data sets when determining a baseline value for a subject or group.}
\value{Boolean vector or vector of indices.}
\author{Vinh Nguyen}
\arguments{\item{x}{A vector where the values are grouped, such as ID in a longitudinal data set.}
\item{index}{If \code{TRUE}, the index of the first element in each "group" is returned; if \code{FALSE}, a boolean vector is returned to indicate where the first elements are at; defaults to \code{FALSE}}
}
\examples{x <- c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4)
First(x, index=FALSE)
First(x, index=TRUE)}

