\name{Latex2html}
\alias{Latex2html}
\title{LaTeX to html}
\usage{Latex2html(tex.files, big.out.file)
}
\description{Convert LaTeX file(s) to html.  The resulting html results can be copied and pasted into a word processing program such as OpenOffice/LibreOffice or MS Word.}
\details{Convert ".tex" files specified in \code{tex.files} to ".html" files via the \code{tth} program.  The naming of the ".html" files are taken from the ".tex" files.

If one big LaTeX file is desired by concatenating the ".tex" files
found in \code{tex.files}, then specify the html file name in
\code{big.out.file}.  This is useful if the LaTeX files each contain
code to a table and one file containing all the tables are desired.

NOTE: System must be *nix with access to the commands \code{cat}, \code{|}, \code{echo}, and \code{tth}.}
\value{Nothing.}
\author{Vinh Nguyen}
\references{http://hutchinson.belmont.ma.us/tth/
http://biostat.mc.vanderbilt.edu/wiki/Main/StatReport}
\arguments{\item{tex.files}{Vector of LaTeX filenames, ending in ".tex".}
\item{big.out.file}{Optional html file name if the multiple LaTeX files specified in \code{tex.files} should be concatenated .}
}

