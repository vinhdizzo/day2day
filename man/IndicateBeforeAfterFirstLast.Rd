\name{IndicateBeforeAfterFirstLast}
\alias{BeforeFirstTrue}
\title{Indicate Before/After First/Last TRUE in groups}
\usage{BeforeFirstTrue(TF, group, strict)
AfterFirstTrue(TF, group, strict)
BeforeLastTrue(TF, group, strict)
AfterLastTrue(TF, group, strict)}
\description{Indicate observations before/after the first/last \code{TRUE} value for each group.}
\details{It is useful in longitudinal studies to subset each group to before or after an incident has occured.  These functions help make the subsetting easy.}
\alias{BeforeFirstTrue}
\alias{AfterFirstTrue}
\alias{BeforeLastTrue}
\alias{AfterLastTrue}
\value{Vector of indicators (logical).}
\author{Vinh Nguyen}
\arguments{\item{TF}{Vector of Boolean values.}
\item{group}{Group/ID indicator, assumed grouped next to each other.}
\item{strict}{\code{TRUE} or \code{FALSE} corresponds to strictly before/after.}
}
\examples{group <- c(1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)
TF <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
data.frame(group, TF, x=BeforeFirstTrue(TF, group, strict=FALSE), y=BeforeFirstTrue(TF, group, strict=TRUE))
data.frame(group, TF, x=AfterFirstTrue(TF, group, strict=FALSE), y=AfterFirstTrue(TF, group, strict=TRUE))
data.frame(group, TF, x=BeforeLastTrue(TF, group, strict=FALSE), y=BeforeLastTrue(TF, group, strict=TRUE))
data.frame(group, TF, x=AfterLastTrue(TF, group, strict=FALSE), y=AfterLastTrue(TF, group, strict=TRUE))}

