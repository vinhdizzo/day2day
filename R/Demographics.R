##' Default \code{continuous.summary.function} for use in \code{\link{SummarizeVar}}.  Returns formatted text of mean plus/minus standard deviation, possibly by group.
##'
##' For use in construction of demographics tables.
##' @title Summarize a continuous vector with mean plus/minus standard deviation
##' @param x Vector of values.
##' @param group Group identifiers to return summaries by group.
##' @param decimal The number of decimal values to format the results; defaults to 2.
##' @param latex Return LaTeX characters if \code{TRUE}; for example, the LaTeX code for the plus-minus symbol.
##' @param na.rm Remove missing values if \code{TRUE}.
##' @param ... Nothing.
##' @return Formatted text of mean plus/minus standard deviation in a vector or matrix.
##' @author Vinh Nguyen
##' @examples
##' SummarizeContinuousDefault(x=c(rnorm(100, 5), rnorm(100, 0)), group=rep(0:1, each=100))
SummarizeContinuousDefault <- function(x, group=rep(1, length(x)), decimal=2, latex=TRUE, na.rm=TRUE, ...){
  mu <- formatC(tapply(x, group, mean, na.rm=na.rm), format="f", digits=decimal)
  sd <- formatC(tapply(x, group, sd, na.rm=na.rm), format="f", digits=decimal)
  return(paste(mu, ifelse(latex, "$\\pm$", "\u00B1"), sd, sep=" ")) ## ±
}

##' Default \code{factor.summary.function} for use in \code{\link{SummarizeVar}}.  Returns formatted text of count and precentages.
##'
##' For use in construction of demographics tables.
##' @title Summarize a factor vector with count and precentages
##' @param x Vector of values.
##' @param group Group identifiers to return summaries by group.
##' @param decimal The number of decimal values to format the results; defaults to 0.
##' @param useNA Defaults to \code{ifany} and passed to \code{\link{table}}.
##' @param latex Return LaTeX characters if \code{TRUE}; for example, the LaTeX code for the percentage symbol.
##' @param ... Nothing.
##' @return Formatted text of counts with percentages in parentheses, in a vector or matrix.
##' @author Vinh Nguyen
##' @examples
##' SummarizeFactorDefault(x=c(sample(1:5, 100, replace=TRUE), sample(1:5, 100, replace=TRUE)), group=rep(0:1, each=100))
SummarizeFactorDefault <- function(x, group=rep(1, length(x)), decimal=0, useNA="ifany", latex=TRUE, ...){
  counts <- table(x, group, useNA=useNA)
  pct <- formatC(counts / matrix(colSums(counts), nrow=nrow(counts), ncol=ncol(counts), byrow=TRUE)*100, format="f", digits=decimal)
  rslt <- matrix(paste(counts, " (", pct, ifelse(latex, "\\%", "%"), ")", sep=""), ncol=ncol(counts))
  rownames(rslt) <- rownames(counts)
  colnames(rslt) <- colnames(counts)
  return(rslt)
}

##' Summarize a continuous or discrete (factor) vector.
##' @title Summarize a vector (continuous or factor)
##' @param x Vector of values.
##' @param group Group identifiers to return summaries by group.
##' @param continuous.summary.function Function to use to summarize a continuous variable; defaults to \code{\link{SummarizeContinuousDefault}}.
##' @param factor.summary.function Function to use to summarize a factor variable; defaults to \code{\link{SummarizeFactorDefault}}.
##' @param latex Return LaTeX characters if \code{TRUE}; for example, the LaTeX code for the percentage symbol.
##' @param ... Arguments to be passed to \code{continuous.summary.function} and \code{factor.summary.function}.
##' @return Formatted text in a vector or matrix.
##' @author Vinh Nguyen
SummarizeVar <- function(x, group=rep(1, length(x)), continuous.summary.function=SummarizeContinuousDefault, factor.summary.function=SummarizeFactorDefault, latex=TRUE, ...){
  if(any(is.na(group))) stop("group must not contain NA.")
  if(is.numeric(x)) rslt <- continuous.summary.function(x, group, latex, ...)
  else if(is.factor(x)) rslt <- factor.summary.function(x, group, latex, ...)
  else stop("x needs to be numeric or factor.")
  return(rslt)
}

