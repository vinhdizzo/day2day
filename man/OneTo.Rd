\name{OneTo}
\alias{OneTo}
\title{One to x}
\usage{OneTo(x)
}
\description{Create a vector containing 1 to the specified value.}
\details{An efficient way to enumerate 1 to a specified value or values.  If there are multiple values, then vectors are created and the numbers get stacked into one vector.}
\value{Vector of positive integers of length \code{sum(x)}.}
\author{Vinh Nguyen}
\arguments{\item{x}{Vector of positive integers.}
}
\examples{OneTo(1:5) ; OneTo( c(1,10,2) )}

