\name{SummarizeVar}
\alias{SummarizeVar}
\title{Summarize a vector (continuous or factor)}
\usage{SummarizeVar(x, group=rep(1, length(x)), latex=TRUE, decimal.factor=0,
    decimal.continuous=2,
    continuous.summary.function=SummarizeContinuousDefault,
    factor.summary.function=SummarizeFactorDefault, ...)
}
\description{Summarize a continuous or discrete (factor) vector.}
\value{Formatted text in a vector or matrix.}
\author{Vinh Nguyen}
\arguments{\item{x}{Vector of values.}
\item{group}{Group identifiers to return summaries by group.}
\item{latex}{Return LaTeX characters if \code{TRUE} (default).  For example, the LaTeX code for the percentage symbol should be preceeded by the escape character \code{\}.}
\item{decimal.factor}{The number of decimals to display in percentages for factor variables.  This is passed to the \code{decimal} in \code{factor.summary.function}.}
\item{decimal.continuous}{The number of decimals to display in percentages for numeric variables.  This is passed to the \code{decimal} in \code{continuous.summary.function}.}
\item{continuous.summary.function}{Function to use to summarize a continuous variable; defaults to \code{\link{SummarizeContinuousDefault}}.  Function must take in the following arguments:
\code{x}: a vector of values.
\code{group}: a vector that identifies group.
\code{decimal}: a numeric value to indicate the decimal places in the formatted output.
\code{latex}: a logical value that indicates whether the resulting output contains LaTeX code; should default to \code{TRUE}.
\code{...}: additional arguments.}
\item{factor.summary.function}{Function to use to summarize a factor variable; defaults to \code{\link{SummarizeFactorDefault}}.  See \code{continuous.summary.function}.}
\item{...}{Arguments to be passed to \code{continuous.summary.function} and \code{factor.summary.function}.}
}

