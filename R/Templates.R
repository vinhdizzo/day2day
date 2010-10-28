##' Copy a directory from an installed package's base directory to the current directory.
##' @title Copy directory from a package
##' @param name Name of new directory.
##' @param dir Path of directory within the installed package's base directory; e.g., \code{"doc"} (2 level directories will not work).
##' @param package Name of package that to take \code{dir} from.
##' @return Nothing.
##' @author Adapted from the \code{create.project} function in the \code{ProjectTemplate} package by John Myles White (http://github.com/johnmyleswhite/ProjectTemplate)
CopyDir <- function(name, dir, package)
{
  tmp.dir <- paste(name, '_tmp', sep = '')

  if (file.exists(name) || file.exists(tmp.dir))
  {
    stop(paste("Cannot create directory", name, "or", tmp.dir, "in working directory."))
  }
  
  dir.create(tmp.dir)
  
  file.copy(system.file(dir, package = package),
            file.path(tmp.dir),
            recursive = TRUE)
            
  file.rename(file.path(tmp.dir, dir),
              name)
  
  unlink(tmp.dir, recursive = TRUE)
}

##' Create a project directory from a template.
##' @title Project Template
##' @param name Name of the project directory.
##' @return Nothing.
##' @author Vinh Nguyen
ProjectStart <- function(name) {
  CopyDir(name=name, dir="template_project", package="day2day")
}

##' Create a package directory from a template.
##' @title Package Template
##' @param name Name of the package directory.
##' @return Nothing.
##' @author Vinh Nguyen
PackageStart <- function(name) {
  CopyDir(name=name, dir="template_package", package="day2day")
}
