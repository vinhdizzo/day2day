### following taken from seqTrial
##' Return \code{yes} if \code{test} is \code{TRUE}, else return \code{no}.
##' A shorthand way to do \link[base]{Control} flow.
##' @title ifelse1
##' @param test An expression returning \code{\link[base]{logical}}.
##' @param yes Expression that is evaluated if \code{test} is \code{TRUE}.
##' @param no Expression that is evaluated if \code{test} is \code{FALSE}.
##' @return \code{yes} or \code{no}.
##' @export
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
##' @export
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
##' OneTo(1:5) ; OneTo( c(1,10,2) )
##' @export
OneTo <- function(x){
  ## takes in vector of positive integers
  ## returns vector of 
  stopifnot( is.whole(x), x > 0 )
  ## eval(parse(text=
  ##   paste('c(', paste('1:', x, collapse=',', sep=''), ')', sep='')
  ##            ))
  unlist(sapply(x, seq)) ## USE THIS INSTEAD!
}
## OneTo(1:5) ; OneTo( c(1,10,2) )

##' Takes in a string with a separator \code{sep} and returns a vector of string.  This allows for faster input of string data.
##' @title Vector of strings
##' @param string One string that we wish to split into a vector of strings.
##' @param sep Separator; defaults to \code{","}.
##' @return Vector of strings.
##' @author Vinh Nguyen
##' @examples string2vec("a,b,c,d")
##' @export
string2vec <- function(string, sep=","){
  ## takes in a string with a separator
  ## returns a vector of string, each element being the value separated in input
  eval(parse(text=paste('c("', paste(unlist(strsplit(string, split=sep)), collapse='","', sep=''), '")', sep='')))
}
## string2vec('a,b,c,d')

##' Determine whether a vector's elements are grouped nearby if they have the same value.
##'
##' @title Are the vector's elements grouped together?
##' @param x 
##' @return \code{logical}.
##' @author Vinh Nguyen
##' @examples
##' is.grouped(c(1,2,2)) ## TRUE
##' is.grouped(c(1,2,2,1)) ## FALSE
##' @export
is.grouped <- function(x) {
  all(tapply(1:length(x), factor(x, levels=unique(x)), function(x) all(diff(x)==1) ))
}

##' For input vector \code{group} where the values are grouped, determine
##' certain attributes for each group: group size, element enumeration,
##' first element, and last element.  NOTE: Assumes group values are
##' grouped next to each other.
##'
##' NOTE: code{group} is assumed to have group values grouped next to each other.
##' 
##' This function is useful in manipulations of longitudinal data sets,
##' such as determining a baseline value for a subject/group or a value
##' at the last visit.
##'
##' \code{GroupSize} determines the group size for each group.
##'
##' \code{GroupEnum} enumerates the elements of each group from 1 to the group size.
##'
##' \code{First} determines the first element of each group; returns a vector of logicals unless \code{index=TRUE}, which returns the indices.
##'
##' \code{Last} determines the last element of each group; returns a vector of logicals unless \code{index=TRUE}, which returns the indices.
##' @title Attribute Extraction for Groups
##' @rdname Groups
##' @aliases GroupSize GroupEnum First Last
##' @usage
##' GroupSize(group)
##' GroupEnum(group)
##' First(group, index=FALSE)
##' Last(group, index=FALSE)
##' @param group A vector where the values are grouped next to each other, such as ID in a longitudinal data set.
##' @param index For use with \code{First} and \code{Last}.  If \code{TRUE}, the index of the first/last element in each "group" is returned;
##' if \code{FALSE}, a boolean vector is returned to indicate where the first/last elements are at; defaults to \code{FALSE}.
##' @return Boolean vector or vector of indices.
##' @author Vinh Nguyen
##' @seealso \code{\link{Last}}
##' @examples
##' x <- c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4)
##' GroupSize(x)
##' GroupEnum(x)
##' First(x, index=FALSE)
##' First(x, index=TRUE)
##' Last(x, index=FALSE)
##' Last(x, index=FALSE)
##' @export GroupSize GroupEnum First Last
GroupSize <- function(group) {
  if(!is.grouped(group)) stop("Vector's elements are not grouped.")
  tapply(group, factor(group, levels=unique(group)), length)
  ## need to add levels up there because tapply automatically sorts...big error!
}

##' @nord
GroupEnum <- function(group) {
  OneTo(GroupSize(group))
}

##' @nord
First <- function(group, index=FALSE) {
  ## cat("NOTE: 'First' function assumes 'group' is grouped.\n")
  first.indicator <- OneTo(GroupSize(group)) == 1
  if(!index) {
    return(first.indicator)
  } else
  if(index) {
    return(which(first.indicator))
  }
}

##' @nord
Last <- function(group, index=FALSE) {
  ## cat("NOTE: 'Last' function assumes 'group' is grouped.\n")
  length.of.each.group <- GroupSize(group)
  last.indicator <- OneTo(GroupSize(group)) == rep(length.of.each.group, times=length.of.each.group)
  if(!index) {
    return(last.indicator)
  } else
  if(index) {
    return(which(last.indicator))
  }
}


##' Indicate observations before/after the first/last \code{TRUE} value for each group.
##'
##' It is useful in longitudinal studies to subset each group to before or after an incident has occured.  These functions help make the subsetting easy.
##' @title Indicate Before/After First/Last TRUE in groups
##' @rdname IndicateBeforeAfterFirstLast
##' @aliases BeforeFirstTrue AfterFirstTrue BeforeLastTrue AfterLastTrue
##' @usage
##' BeforeFirstTrue(TF, group, strict)
##' AfterFirstTrue(TF, group, strict)
##' BeforeLastTrue(TF, group, strict)
##' AfterLastTrue(TF, group, strict)
##' @param TF Vector of Boolean values.
##' @param group Group/ID indicator, assumed grouped next to each other.
##' @param strict \code{TRUE} or \code{FALSE} corresponds to strictly before/after.
##' @return Vector of indicators (logical).
##' @author Vinh Nguyen
##' @examples
##' group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)
##' TF <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
##' data.frame(group, TF, x=BeforeFirstTrue(TF, group, strict=FALSE), y=BeforeFirstTrue(TF, group, strict=TRUE))
##' data.frame(group, TF, x=AfterFirstTrue(TF, group, strict=FALSE), y=AfterFirstTrue(TF, group, strict=TRUE))
##' data.frame(group, TF, x=BeforeLastTrue(TF, group, strict=FALSE), y=BeforeLastTrue(TF, group, strict=TRUE))
##' data.frame(group, TF, x=AfterLastTrue(TF, group, strict=FALSE), y=AfterLastTrue(TF, group, strict=TRUE))
##' @export BeforeFirstTrue AfterFirstTrue BeforeLastTrue AfterLastTrue
BeforeFirstTrue <- function(TF, group, strict=FALSE) {
  if(length(TF) != length(group)) stop("TF and group must be of the same length.")
  observation.time <- GroupEnum(group)
  first.true <- tapply(TF, factor(group, levels=unique(group)), function(x){ ifelse1(any(x), which(x)[1], Inf) })
  first.true.repeated <- rep(first.true, times=GroupSize(group))
  return(ifelse1(strict, observation.time < first.true.repeated, observation.time <= first.true.repeated))
}
## group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6); TF <- c(F, T, T, F, F, F, T, T, T, T, F, T, F, T, F)
## data.frame(group, TF, x=BeforeFirstTrue(TF, group, strict=FALSE), y=BeforeFirstTrue(TF, group, strict=TRUE))

##' @nord
AfterFirstTrue <- function(TF, group, strict=TRUE) {
  !BeforeFirstTrue(TF, group, !strict)
}
## group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6); TF <- c(F, T, T, F, F, F, T, T, T, T, F, T, F, T, F)
## data.frame(group, TF, x=AfterFirstTrue(TF, group, strict=FALSE), y=AfterFirstTrue(TF, group, strict=TRUE))

##' @nord
BeforeLastTrue <- function(TF, group, strict=FALSE) {
  if(length(TF) != length(group)) stop("TF and group must be of the same length.")
  observation.time <- GroupEnum(group)
  last.true <- tapply(TF, factor(group, levels=unique(group)), function(x){ ifelse1(any(x), {trues <- which(x); trues[length(trues)]}, Inf) })
  last.true.repeated <- rep(last.true, times=GroupSize(group))
  return(ifelse1(strict, observation.time < last.true.repeated, observation.time <= last.true.repeated))
}
## group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6); TF <- c(F, T, T, F, F, F, T, T, T, T, F, T, F, T, F)
## data.frame(group, TF, x=BeforeLastTrue(TF, group, strict=FALSE), y=BeforeLastTrue(TF, group, strict=TRUE))

##' @nord
AfterLastTrue <- function(TF, group, strict=TRUE) {
  !BeforeLastTrue(TF, group, !strict)
}
## group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6); TF <- c(F, T, T, F, F, F, T, T, T, T, F, T, F, T, F)
## data.frame(group, TF, x=AfterLastTrue(TF, group, strict=FALSE), y=AfterLastTrue(TF, group, strict=TRUE))

##' Paste columns of a matrix or data frame together.  See \code{\link{paste}}.
##' @title Paste columns of a matrix or data frame
##' @param x A matrix or data frame.
##' @param sep The delimiter to concatenate the columns of \code{x} together.
##' @return A vector of strings.
##' @author Vinh Nguyen
##' @examples
##' PasteColumn(cbind(rep(1:10), rep(1:10)))
##' @export
PasteColumn <- function(x, sep=" "){
  ##stopifnot(is.data.frame(x) | is.matrix(x))
  sep2 <- sep
  if(is.data.frame(x) | is.matrix(x)){
    n <- ncol(x)
    string2eval <- paste("paste(", paste("x[, ", 1:n, "]", sep="", collapse=", "), ", sep=\"", sep2, "\"", ")", sep="")
    rslt <- eval(parse(text=string2eval))
  } else
  if(is.vector(x)){
    rslt <- paste(x, collapse=sep2)
  }
  return(rslt)
}

## following taken from http://4dpiecharts.com/2011/01/20/bad-kitty/
## Richie Cotton
catn <- function(...) cat(..., "\n")
##catn("Yes, I would like this content on its own line.")
cats <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
{
  cat(sprintf(...), file = file, sep = sep, fill = fill, labels = labels, append = append)
}
#Or, combining the two ideas
catsn <- function(...) catn(cats(...))
##catsn("The temperature is %g Celcius in %s", -4, "Buxton")

## http://www.markmfredrickson.com/thoughts/2011-02-06-peeking-inside-r-functions.html
## Mark M. Fredickson mark.m.fredrickson@gmail.com
fnpeek <- function(f, name = NULL) {
  env <- environment(f)
  if (is.null(name)) {
    return(ls(envir = env))
  }
  if (name %in% ls(envir = env)) {
    return(get(name, env))
  }
  return(NULL)
}


