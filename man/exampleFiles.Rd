\name{exampleFiles}
\alias{exampleFiles}
\alias{examplePages}
\title{ Path to Files in the Examples Subdirectory }
\description{
  Given one or more file names, returns the corresponding paths to files in the \code{"Examples"} subdirectory of a package.  With the \code{names} argument missing, returns the names of all such files.
}
\usage{
exampleFiles(names = character(), where = "SoDA", oneFile = FALSE, path = TRUE)
}
\arguments{
  \item{names}{Either character string names or page numbers matched
      against the file names defined by the second argument.  See
      details below.}
  \item{where}{The name of the package containing the
      \code{"Examples"} directory, by default, this package, or else
      the path name of a directory which itself contains the files of interest. }
  \item{oneFile}{ If \code{TRUE}, and more than one file matches, the
      caller will be asked to select one of these files. }
  \item{path}{  If \code{TRUE}, the file(s) will be returned with full
    directory paths; else, only with the file names.}
}
\details{
  In any call, argument \code{names} can be a vector of character
  string names, expected to match files in the directory of examples.
  The names may match only the leading part of the file name; in
  particular, it will often be the case that the actual file name has
  a suffix such as \code{".R"}, omitted in the \code{names} argument.

  If the argument \code{where} is a package name, then that
  package is expected to have a directory named \code{"Examples"}, as
  does this package.  In all other circumstances, this argument must
  itself be the path name of a directory containing the example files.

  In the case of a package, if the package has a data frame object
  named \code{examplePages}, that object will be used to match page
  numbers of a document on which the examples appear.  In the default
  case, the document is the book \emph{Software for Data Analysis}.
  If you see an example in the book that appears to be the output from
  running some \R code, enter the page number on which the output
  starts as the \code{names} argument.  Often (though not always) the
  corresponding \R code is one of the files in the \code{Examples}
  directory of the package.  Some other files, such as source in other
  languages or related data, are also provided
  corresponding to examples.

  Note that the same use of page numbers applies in calls to
  \code{\link{runExample}} or \code{\link{demoExample}}.
}
\value{
  Names for the matching files, or for all such files if \code{names} was missing.
}
\seealso{  \code{\link{runExample}} to run such files, and
  \code{\link{demoExample}} to run them as interactive demos. }
\keyword{programming}


