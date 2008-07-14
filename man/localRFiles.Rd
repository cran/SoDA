\name{localRFiles}
\alias{localRFiles}
\alias{menuRFile}
\title{The R source files in the local directory}
\description{
  Returns the names of the R source files in a directory, by default the current working directory.
}
\usage{
localRFiles(directory = getwd(), suffix = "[.][RSq]$", ask = FALSE)
}
\arguments{
  \item{directory}{ Where to look, by default the directory in which R is currently running}
  \item{suffix}{Regular expression to identify R source files.}
  \item{ask}{If \code{TRUE}, uses the menu function to prompt the user for one of the local R files or  to enter a file name.}
}
\value{
  The character vector of matching file names.

\code{menuRFile} 
}
\author{John M. Chambers}
\examples{
## all the example files for this package
localRFiles(system.file("R-ex",package="SoDA"))
}
\keyword{programming}

