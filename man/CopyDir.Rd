\name{CopyDir}
\alias{CopyDir}
\title{Copy directory from a package}
\usage{CopyDir(name, dir, package)
}
\description{Copy a directory from an installed package's base directory to the current directory.}
\value{Nothing.}
\author{Adapted from the \code{create.project} function in the \code{ProjectTemplate} package by John Myles White (http://github.com/johnmyleswhite/ProjectTemplate)}
\arguments{\item{name}{Name of new directory.}
\item{dir}{Path of directory within the installed package's base directory; e.g., \code{"doc"} (2 level directories will not work).}
\item{package}{Name of package that to take \code{dir} from.}
}

