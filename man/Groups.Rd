\name{Groups}
\alias{GroupSize}
\title{Attribute Extraction for Groups}
\usage{GroupSize(group)
GroupEnum(group)
First(group, index=FALSE)
Last(group, index=FALSE)}
\description{For input vector \code{group} where the values are grouped, determine
certain attributes for each group: group size, element enumeration,
first element, and last element.  NOTE: Assumes group values are
grouped next to each other.}
\details{NOTE: code{group} is assumed to have group values grouped next to each other.

This function is useful in manipulations of longitudinal data sets,
such as determining a baseline value for a subject/group or a value
at the last visit.

\code{GroupSize} determines the group size for each group.

\code{GroupEnum} enumerates the elements of each group from 1 to the group size.

\code{First} determines the first element of each group; returns a vector of logicals unless \code{index=TRUE}, which returns the indices.

\code{Last} determines the last element of each group; returns a vector of logicals unless \code{index=TRUE}, which returns the indices.}
\alias{GroupSize}
\alias{GroupEnum}
\alias{First}
\alias{Last}
\value{Boolean vector or vector of indices.}
\author{Vinh Nguyen}
\seealso{\code{\link{Last}}}
\arguments{\item{group}{A vector where the values are grouped next to each other, such as ID in a longitudinal data set.}
\item{index}{For use with \code{First} and \code{Last}.  If \code{TRUE}, the index of the first/last element in each "group" is returned;
if \code{FALSE}, a boolean vector is returned to indicate where the first/last elements are at; defaults to \code{FALSE}.}
}
\examples{x <- c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4)
GroupSize(x)
GroupEnum(x)
First(x, index=FALSE)
First(x, index=TRUE)
Last(x, index=FALSE)
Last(x, index=FALSE)}

