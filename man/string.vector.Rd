\name{string.vector}
\alias{string.vector}
\title{Vector of strings}
\usage{string.vector(string, sep=",")
}
\description{Takes in a string with a separator \code{sep} and returns a vector of string.  This allows for faster input of string data.}
\value{Vector of strings.}
\author{Vinh Nguyen}
\arguments{\item{string}{One string that we wish to split into a vector of strings.}
\item{sep}{Separator; defaults to \code{","}.}
}
\examples{string.vector("a,b,c,d")}

