#### Functions that assist in report generation / reproducible research.

##' Print to console: \code{ \\\\} followed by a new line.
##' @title LaTeX: New Line
##' @param n Number of new lines with \code{ \\\\}; defaults to 1.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' LatexNewLine()
LatexNewLine <- function(n=1, ...){
  cat(rep(" \\\\\n", n))
}

##' Print to console: \code{"\hline"} followed by a new line.
##' @title LaTeX: Horizontal Line(s)
##' @param n Number of horizontal lines.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' hline(1); hline(2)
hline <- function(n=1, ...) {
  cat(paste(rep("\\hline", n), collapse="\n"))
  cat("\n")
}

##' Print to console: begin table environment.
##' @title LaTeX: Begin Table
##' @param loc Location of table, e.g, \code{htb}; defaults to nothing.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' BeginTable() ; BeginTable("htb")
BeginTable <- function(loc, ...) {
  if(missing(loc)) {
    cat("\\begin{table}\n")
  } else {
    cat(paste("\\begin{table}[", loc, "]\n", sep=""))
  }
}

##' Print to console: begin tabular environment.
##' @title LaTeX: Begin Tabular
##' @param align Tabular alignment, e.g., \code{"lrr"}
##' @param hline.num Number of horizontal lines following tabular statement; defaults to 1.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' BeginTabular("lrr"); BeginTabular("lrr", 2)
BeginTabular <- function(align, hline.num=1, ...) {
  cat(paste("\\begin{tabular}{", align, "}\n", sep=""))
  hline(hline.num)
}

##' Print to console: caption.
##' @title LaTeX: Caption
##' @param caption String for caption.
##' @param ...  Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' caption("Baseline demographics.")
caption <- function(caption, ...) {
  cat("\\caption{", caption, "}\n", sep="")
}

##' Print to console: label.
##' @title LaTeX: Label
##' @param label String for label.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' label("tab:demographics")
label <- function(label, ...) {
  cat("\\label{", label, "}\n", sep="")
}

##' Print to console: end tabular environment.
##' @title LaTeX: End Tabular
##' @param hline.num Number of horizontal lines before tabular statement; defaults to 1.
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' EndTabular(1); EndTabular(2)
EndTabular <- function(hline.num=1, ...) {
  hline(hline.num)
  cat("\\end{tabular}\n")
}

##' Print to console: end table environment.
##' @title LaTeX: End table
##' @param ... Nothing.
##' @return Nothing.
##' @author Vinh Nguyen
##' @examples
##' EndTable()
EndTable <- function(...) {
  cat("\\end{table}\n")
}

##' Generate a \code{multicolumn} statement for used with \code{paste} and \code{cat} in table LaTeX table generation.
##' @title LaTeX: multicolumn
##' @param text Text to be displayed in table cell.
##' @param ncol Number of columns to span.
##' @param align Alignment, e.g., "c" for centered.
##' @return Character value of multicolumn statement.
##' @author Vinh Nguyen
##' @examples
##' multicolumn("Hello world!", ncol=3)
multicolumn <- function(text, ncol, align="c") {
  stopifnot(is.numeric(ncol))
  paste("\\multicolumn{", ncol, "}{", align, "}{", text, "}", sep="")
}

#### Following deprecated
## Convert LaTeX file(s) to html.  The resulting html results can be copied and pasted into a word processing program such as OpenOffice/LibreOffice or MS Word.
##
## Convert ".tex" files specified in \code{tex.files} to ".html" files via the \code{tth} program.  The naming of the ".html" files are taken from the ".tex" files.
##
## If one big LaTeX file is desired by concatenating the ".tex" files
## found in \code{tex.files}, then specify the html file name in
## \code{big.out.file}.  This is useful if the LaTeX files each contain
## code to a table and one file containing all the tables are desired.
##
## NOTE: System must be *nix with access to the commands \code{cat}, \code{|}, \code{echo}, and \code{tth}.
## @title LaTeX to html
## @param tex.files Vector of LaTeX filenames, ending in ".tex".
## @param big.out.file Optional html file name if the multiple LaTeX files specified in \code{tex.files} should be concatenated .
## @return Nothing.
## @author Vinh Nguyen
## @references
## http://hutchinson.belmont.ma.us/tth/
## http://biostat.mc.vanderbilt.edu/wiki/Main/StatReport
## Latex2html <- function(tex.files, big.out.file)
## {
##   require(R.utils) ## tolower() function
##   if(!all(tolower(substr(tex.files, start=nchar(tex.files)-3, nchar(tex.files))) == ".tex")) stop("Not all files in tex.files end in '.tex'.")
##   if(length(tex.files)>1) stopifnot(!missing(big.out.file))
##   base.names <- substr(tex.files, start=1, nchar(tex.files)-4)
##   html.files <- paste(base.names, "html", sep=".")
##   for(i in 1:length(tex.files)){
##     system(paste("echo '\\documentclass{article}' | cat -", tex.files[i], "| tth >", html.files[i], sep=" ")) ## need \documentclass{article} to get caption working
##     cat("GENERATED:", html.files[i], "\n")
##   }
##   ##pdfFiles <- paste(baseName, "pdf", sep=".")
##   ##return(cbind(base.names, html.files))
##   if(!missing(big.out.file)){
##     system(paste("echo '\\documentclass{article}' | cat -", paste(tex.files, collapse=" "), "| tth >", big.out.file, sep=" ")) ## need \documentclass{article} to get caption working
##     cat("GENERATED:", big.out.file, "\n")
##   }
##   invisible(NULL)
## }


##' Convert a LaTeX file to html.
##'
##' Convert a \code{.tex} file to a \code{.html} file via \code{htlatex} from \code{tex4ht}.  This function will only work on *nix platforms.
##' @title LaTeX to html
##' @param file A file ending in \code{.tex}.
##' @return Nothing.
##' @author Vinh Nguyen
##' @references
##' http://www.tug.org/applications/tex4ht/mn.html
##' http://biostat.mc.vanderbilt.edu/wiki/Main/SweaveConvert
latex2html <- function(file) {
  for(f in files){
    system(paste("htlatex", shQuote(f), sep=" "))
  }
  invisible(NULL)
}

##' Merge multiple .tex files and .eps files into one LaTeX file (.tex) file, and convert the LaTeX file into .odt (OpenOffice) and .doc (Word).
##'
##' This function makes use of \code{latex}, \code{mk4ht oolatex} from \code{tex4ht} and \code{ooconvert}.  This function will only work on *nix platforms.
##' @title Merge LaTeX files and figures into doc
##' @param tex.files Vector of filenames ending in {.tex}.  Files must not contain the document header and "end document" as they will be merged; header and ending will be added on the fly.  Typically usage would include multiple files containing LaTeX table codes.
##' @param figures Vector of encapsulated postscript (\code{eps}) filenames to be included in the final document.  \code{eps} files are needed since \code{mk4ht oolatex} generates {dvi} files from the \code{latex} command, and only \code{eps} files are supported with the \code{latex} command.  Path to files must not contain spaces (limitations of \code{graphics} in \code{latex}).
##' @param figure.captions Vector of caption strings that correspond to \code{figures}.
##' @param out.file  Name of the out file without the extensions.  The path to the file must not contain spaces (limitation of \code{mk4ht oolatex}).
##' @param latex.packages Vector of packages to be included in LaTeX document.  \code{graphicx} and \code{multirow} are always included by default.
##' @return Nothing.
##' @author Vinh Nguyen
##' @references
##' http://www.tug.org/applications/tex4ht/mn.html
##' http://ooconvert.sourceforge.net/
##' http://biostat.mc.vanderbilt.edu/wiki/Main/SweaveConvert
MergeLatex2Doc <- function(tex.files, figures, figure.captions, out.file, latex.packages)
{
  require(R.utils) ## tolower() and getAbsolutePath()
  stopifnot(!missing(tex.files) | !missing(figures))
  stopifnot(!missing(out.file))
  if(length(grep(" ", out.file))!=0) {
    stop("Path of out.file must not contain space (limitation of 'mk4ht oolatex' from tex4ht package).")
  }
  out.file.tex <- path.expand(getAbsolutePath(paste(out.file, "tex", sep=".")))
  out.file.odt <- path.expand(getAbsolutePath(paste(out.file, "odt", sep=".")))
  out.file.doc <- path.expand(getAbsolutePath(paste(out.file, "doc", sep=".")))
  begin <- paste("\\documentclass{article}\n\\usepackage{graphicx}\n\\usepackage{multirow}", paste("\\usepackage{", latex.packages, "}", collapse="\n"), "\\begin{document}\n", sep="\n")
  end <- "\\end{document}"
  begin.file <- tempfile(pattern="begin")
  end.file <- tempfile(pattern="end")
  cat(begin, file=begin.file)
  cat(end, file=end.file)
  if(!missing(figures)){
    if(!all(tolower(substr(figures, start=nchar(figures)-3, nchar(figures))) == ".eps")){
      stop("Figures must end in '.eps'.")
    }
    if(length(grep(" ", figures))!=0) {
      stop("Path of figures must not contain space (limitation of graphics package in LaTeX); see http://www.tex.ac.uk/cgi-bin/texfaq2html?label=grffilenames")
    }
    if(!missing(figure.captions)) {
      stopifnot(length(figures) == length(figure.captions))
    }
    fig.file <- tempfile(pattern="figures")
    for(i in 1:length(figures)){
      cat("\\begin{figure}\n", file=fig.file, append=TRUE)
      cat("\\includegraphics{", path.expand(getAbsolutePath(figures[i])), "}\n", sep="", file=fig.file, append=TRUE) ## need absolute path for html images ## wanted absolute path, but cannot have space
      ## cat("\\includegraphics{", figures[i], "}\n", sep="", file=fig.file, append=TRUE)
      if(!missing(figure.captions)) {
        cat("\\caption{", figure.captions[i], "}\n", file=fig.file, append=TRUE)
      }
      cat("\\end{figure}\n", file=fig.file, append=TRUE)
    }
  }
  if(!missing(tex.files)) {
    if(!all(tolower(substr(out.file, start=nchar(out.file)-3, nchar(out.file))) != ".tex")){
      stop("Not all tex.files end in '.tex'.")
    }
    for(i in 1:length(tex.files)) {
      tex.files[i] <- path.expand(getAbsolutePath(tex.files[i]))
    }
  }
  cd <- paste("cd", shQuote(dirname(getAbsolutePath(out.file.doc))), sep=" ")
  system(paste(cd, "&&", "cat", shQuote(begin.file), ifelse(missing(tex.files), "", paste(shQuote(tex.files), collapse=" ")), ifelse(missing(figures), "", shQuote(fig.file)), shQuote(end.file), ">", shQuote(out.file.tex), "&&", "mk4ht oolatex", shQuote(out.file.tex), "&&", "ooconvert", shQuote(out.file.odt), shQuote(out.file.doc), sep=" "))
  cat("GENERATED: ", out.file, "\n")
  invisible(NULL)
}


