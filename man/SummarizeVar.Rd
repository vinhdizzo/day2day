\name{SummarizeVar}
\alias{SummarizeVar}
\title{Summarize a vector (continuous or factor)}
\usage{SummarizeVar(x, group=rep(1, length(x)),
    continuous.summary.function=SummarizeContinuousDefault,
    factor.summary.function=SummarizeFactorDefault, latex=TRUE, ...)
}
\description{Summarize a continuous or discrete (factor) vector.}
\value{Formatted text in a vector or matrix.}
\author{Vinh Nguyen}
\arguments{\item{x}{Vector of values.}
\item{group}{Group identifiers to return summaries by group.}
\item{continuous.summary.function}{Function to use to summarize a continuous variable; defaults to \code{\link{SummarizeContinuousDefault}}.}
\item{factor.summary.function}{Function to use to summarize a factor variable; defaults to \code{\link{SummarizeFactorDefault}}.}
\item{latex}{Return LaTeX characters if \code{TRUE}; for example, the LaTeX code for the percentage symbol.}
\item{...}{Arguments to be passed to \code{continuous.summary.function} and \code{factor.summary.function}.}
}

