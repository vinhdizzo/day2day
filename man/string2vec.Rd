\name{string2vec}
\alias{string2vec}
\title{Vector of strings}
\usage{string2vec(string, sep=",")
}
\description{Takes in a string with a separator \code{sep} and returns a vector of string.  This allows for faster input of string data.}
\value{Vector of strings.}
\author{Vinh Nguyen}
\arguments{\item{string}{One string that we wish to split into a vector of strings.}
\item{sep}{Separator; defaults to \code{","}.}
}
\examples{string2vec("a,b,c,d")}

