\name{SummarizeFactorDefault}
\alias{SummarizeFactorDefault}
\title{Summarize a factor vector with count and precentages}
\usage{SummarizeFactorDefault(x, group=rep(1, length(x)), decimal=0, useNA="ifany", latex=TRUE, ...)
}
\description{Default \code{factor.summary.function} for use in \code{\link{SummarizeVar}}.  Returns formatted text of count and precentages.}
\details{For use in construction of demographics tables.}
\value{Formatted text of counts with percentages in parentheses, in a vector or matrix.}
\author{Vinh Nguyen}
\arguments{\item{x}{Vector of values.}
\item{group}{Group identifiers to return summaries by group.}
\item{decimal}{The number of decimal values to format the results; defaults to 0.}
\item{useNA}{Defaults to \code{ifany} and passed to \code{\link{table}}.}
\item{latex}{Return LaTeX characters if \code{TRUE}; for example, the LaTeX code for the percentage symbol.}
\item{...}{Nothing.}
}
\examples{SummarizeFactorDefault(x=c(sample(1:5, 100, replace=TRUE), sample(1:5, 100, replace=TRUE)), group=rep(0:1, each=100))}

