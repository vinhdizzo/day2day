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

##' Convert LaTeX file(s) to html.  The resulting html results can be copied and pasted into a word processing program such as OpenOffice/LibreOffice or MS Word.
##'
##' Convert ".tex" files specified in \code{tex.files} to ".html" files via the \code{tth} program.  The naming of the ".html" files are taken from the ".tex" files.
##'
##' If one big LaTeX file is desired by concatenating the ".tex" files
##' found in \code{tex.files}, then specify the html file name in
##' \code{big.out.file}.  This is useful if the LaTeX files each contain
##' code to a table and one file containing all the tables are desired.
##'
##' NOTE: System must be *nix with access to the commands \code{cat}, \code{|}, \code{echo}, and \code{tth}.
##' @title LaTeX to html
##' @param tex.files Vector of LaTeX filenames, ending in ".tex".
##' @param big.out.file Optional html file name if the multiple LaTeX files specified in \code{tex.files} should be concatenated .
##' @return Nothing.
##' @author Vinh Nguyen
##' @references
##' http://hutchinson.belmont.ma.us/tth/
##' http://biostat.mc.vanderbilt.edu/wiki/Main/StatReport
Latex2html <- function(tex.files, big.out.file)
{
  require(R.utils) ## tolower() function
  if(!all(tolower(substr(tex.files, start=nchar(tex.files)-3, nchar(tex.files))) == ".tex")) stop("Not all files in tex.files end in '.tex'.")
  if(length(tex.files)>1) stopifnot(!missing(big.out.file))
  base.names <- substr(tex.files, start=1, nchar(tex.files)-4)
  html.files <- paste(base.names, "html", sep=".")
  for(i in 1:length(tex.files)){
    system(paste("echo '\\documentclass{article}' | cat -", tex.files[i], "| tth >", html.files[i], sep=" ")) ## need \documentclass{article} to get caption working
    cat("GENERATED:", html.files[i], "\n")
  }
  ##pdfFiles <- paste(baseName, "pdf", sep=".")
  ##return(cbind(base.names, html.files))
  if(!missing(big.out.file)){
    system(paste("echo '\\documentclass{article}' | cat -", paste(tex.files, collapse=" "), "| tth >", big.out.file, sep=" ")) ## need \documentclass{article} to get caption working
    cat("GENERATED:", big.out.file, "\n")
  }
  invisible(NULL)
}

##LatexTable2html(c("Table-BaselineCharacteristics.tex", "Table-ResultsPrimary.tex"), "Table.html")
