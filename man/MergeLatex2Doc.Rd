\name{MergeLatex2Doc}
\alias{MergeLatex2Doc}
\title{Merge LaTeX files and figures into doc}
\usage{MergeLatex2Doc(tex.files, figures, figure.captions, out.file, latex.packages)
}
\description{Merge multiple .tex files and .eps files into one LaTeX file (.tex) file,
and convert the LaTeX file into .odt (OpenOffice) and .doc (Word).}
\details{This function makes use of \code{latex}, \code{mk4ht oolatex} from
\code{tex4ht} and \code{ooconvert}.  This function will only work on
*nix platforms.}
\value{Nothing.}
\author{Vinh Nguyen}
\references{http://www.tug.org/applications/tex4ht/mn.html
http://ooconvert.sourceforge.net/
http://biostat.mc.vanderbilt.edu/wiki/Main/SweaveConvert}
\arguments{\item{tex.files}{Vector of filenames ending in {.tex}.  Files must
not contain the document header and "end document" as they will be
merged; header and ending will be added on the fly.  Typically usage
would include multiple files containing LaTeX table codes.}
\item{figures}{Vector of encapsulated postscript (\code{eps})
filenames to be included in the final document.  \code{eps} files are
needed since \code{mk4ht oolatex} generates {dvi} files from the
\code{latex} command, and only \code{eps} files are supported with
the \code{latex} command.  Path to files must not contain spaces
(limitations of \code{graphics} in \code{latex}).}
\item{figure.captions}{Vector of caption strings that correspond to \code{figures}.}
\item{out.file}{Name of the out file without the extensions.  The path to the file must not contain spaces (limitation of \code{mk4ht oolatex}).}
\item{latex.packages}{Vector of packages to be included in LaTeX document.  \code{graphicx} and \code{multirow} are always included by default.}
}

