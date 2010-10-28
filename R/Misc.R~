### following taken from seqTrial
##' Return \code{yes} if \code{test} is \code{TRUE}, else return \code{no}.
##' A shorthand way to do \link[base]{Control} flow.
##' @title ifelse1
##' @param test An expression returning \code{\link[base]{logical}}.
##' @param yes Expression that is evaluated if \code{test} is \code{TRUE}.
##' @param no Expression that is evaluated if \code{test} is \code{FALSE}.
##' @return \code{yes} or \code{no}.
ifelse1 <- function(test, yes, no){
  # Return yes if test is TRUE, else no.
  # Like ifelse(), except that test is not a vector, and yes or no
  #  is returned as is (whatever its length).
  if(test)
    yes
  else
    no
}

##' Determine whether each element of a vector or matrix of numerics is whole.
##' @title Whole Numbers
##' @param x Vector or matrix of numerics.
##' @return Vector or matrix containing \code{\link[base]{logical}} indicators.
##' @author Vinh Nguyen
##' @examples
##' is.whole(1:5) ; is.whole(1) ; is.whole(c(1,1.2))
is.whole <- function(x){
  ## takes in vector/matrix of numerics
  ## returns TRUE/FALSE whether all numbers are whole numbers
  stopifnot(is.numeric(x))
  all(floor(x)==x)
}
## is.whole(1:5) ; is.whole(1) ; is.whole(c(1,1.2))

##' Create a vector containing 1 to the specified value.
##'
##' An efficient way to enumerate 1 to a specified value or values.  If there are multiple values, then vectors are created and the numbers get stacked into one vector.
##' @title One to x
##' @param x Vector of positive integers.
##' @return Vector of positive integers of length \code{sum(x)}.
##' @author Vinh Nguyen
##' @examples
##' oneTo(1:5) ; oneTo( c(1,10,2) )
oneTo <- function(x){
  ## takes in vector of positive integers
  ## returns vector of 
  stopifnot( is.whole(x), x > 0 )
  eval(parse(text=
    paste('c(', paste('1:', x, collapse=',', sep=''), ')', sep='')
             ))
}
## oneTo(1:5) ; oneTo( c(1,10,2) )

##' Takes in a string with a separator \code{sep} and returns a vector of string.  This allows for faster input of string data.
##' @title Vector of strings
##' @param string One string that we wish to split into a vector of strings.
##' @param sep Separator; defaults to \code{","}.
##' @return Vector of strings.
##' @author Vinh Nguyen
##' @examples string.vector("a,b,c,d")
string.vector <- function(string, sep=","){
  ## takes in a string with a separator
  ## returns a vector of string, each element being the value separated in input
  eval(parse(text=paste('c("', paste(unlist(strsplit(string, split=sep)), collapse='","', sep=''), '")', sep='')))
}
## string.vector('a,b,c,d')

##' For input \code{x} where the values are grouped, determine where the first element of each group occurs.
##'
##' This function is useful in longitudinal data sets when determining a baseline value for a subject or group.
##' @title First
##' @param x A vector where the values are grouped, such as ID in a longitudinal data set.
##' @param index If \code{TRUE}, the index of the first element in each "group" is returned; if \code{FALSE}, a boolean vector is returned to indicate where the first elements are at; defaults to \code{FALSE}
##' @return Boolean vector or vector of indices.
##' @author Vinh Nguyen
##' @examples
##' x <- c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4)
##' First(x, index=FALSE)
##' First(x, index=TRUE)
First <- function(x, index=FALSE) {
  cat("NOTE: 'First' function assumes 'x' is grouped.\n")
  first.indicator <- oneTo(tapply(x, x, length)) == 1
  if(!index) {
    return(first.indicator)
  } else
  if(index) {
    return(which(first.indicator))
  }
}

##' For input \code{x} where the values are grouped, determine where the last element of each group occurs.
##'
##' This function is useful in longitudinal data sets when determining a last value for a subject or group.
##' @title Last
##' @param x A vector where the values are grouped, such as ID in a longitudinal data set.
##' @param index If \code{TRUE}, the index of the last element in each "group" is returned; if \code{FALSE}, a boolean vector is returned to indicate where the last elements are at; defaults to \code{FALSE}
##' @return Boolean vector or vector of indices.
##' @author Vinh Nguyen
##' @examples
##' x <- c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4)
##' Last(x, index=FALSE)
##' Last(x, index=TRUE)
Last <- function(x, index=FALSE) {
  cat("NOTE: 'Last' function assumes 'x' is grouped.\n")
  length.of.each.group <- tapply(x, x, length)
  last.indicator <- oneTo(tapply(x, x, length)) == rep(length.of.each.group, times=length.of.each.group)
  if(!index) {
    return(last.indicator)
  } else
  if(index) {
    return(which(last.indicator))
  }
}
