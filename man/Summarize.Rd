\name{Summarize}
\alias{Summarize}
\title{Summarize a Data Set (Demographics)}
\usage{Summarize(formula, data, latex=TRUE, ...)
}
\description{Creates a summary table of a data set in a matrix object for pretty printing via \code{\link[xtable]{xtable}}.}
\details{This is generally used to create demographics table and used with the package \link{xtable} to print.}
\value{A matrix to be used with \code{\link[xtable]{xtable}}, which in turn should be used in \code{\link[xtable]{print.xtable}}.}
\author{Vinh Nguyen}
\arguments{\item{formula}{A \code{\link[stats]{formula}}, with the left-hand side being empty or a group variable to summarize by.  The right-hand side should include variables to summarize by; they should be either continuous variables or factors/characters.}
\item{data}{A \code{\link{data.frame}} where the variables in \code{formula} come from; if not specified, variables are looked for in the parent environment.}
\item{latex}{A \link{logical} variable that determines whether the resulting output will be part of a LaTeX document.  Defaults to \code{TRUE}.}
\item{...}{Additional arguments passed to \link{SummarizeVar}, such as \code{decimal.factor}, \code{decimal.continuous}, \code{continuous.summary.function}, and \code{factor.summary.function}.}
}
\examples{set.seed(1)
n <- 50
df <- data.frame(trt=sample(0:1, 2*n, replace=TRUE), x1=runif(2*n), x2=rnorm(2*n), x3=sample(c("a", "b", "c"), 2*n, replace=TRUE))
Summarize(~x1+x2+x3, data=df)
Summarize(trt~x1+x2+x3, data=df)
Summarize(~., data=df)
Summarize(trt~., data=df, decimal.factor=2)
\dontrun{print(xtable(Summarize(trt~., data=df)), sanitize.text.function=identity)}}

