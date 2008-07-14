\name{runExample}
\alias{runExample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Run or access example files}
\description{
  This function finds and runs an file,  usually of R code.  The
  function is specialized to look for examples included in the package
  and based on material in \emph{Software for Data Analysis}, but can
  be used for other directories containing source files as well.
}
\usage{
runExample(what, where = , run = TRUE, ..., echo = TRUE, prompt.echo, wd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{ Identifies the example to be run.  Can be either a  character string name of the file to be run,
      possibly without the suffix or the page in  \emph{Software for
        Data Analysis} where the example appears.  See
      \code{\link{exampleFiles}} for details. }
  \item{where}{ The character string name of the package or directory
      in which the files are to be found.  By
      default, and usually, it is
      the \code{SoDA} package.  If a package name is given, that
      package must have an \code{"Examples"} directory.}
  \item{run}{ Should the example code be run, or only parsed, assuming
    it is R source code?}
  \item{\dots, echo, prompt.echo}{ Any optional arguments to the \code{\link{source}}
      function, assuming \code{run} is \code{TRUE}. The default for
      \code{prompt.echo} is constructed from an abbreviation of the
      file name. }
  \item{wd}{ The working directory in which to run the example. If the
      argument is missing and \code{where} is a package, the working
      directory is the package directory, which is also the parent
      directory of the \code{Examples} directory.

      If \code{where} is a directory and not a package, and \code{wd}
      is missing, then the working directory is not changed.
      Otherwise, the working directory is set before running the
      example and reset after.  As you would hope, this argument is
      ignored if \code{run} is \code{FALSE}.
      }
}
\value{
  If the file is an R source file, the function returns either the
  value from \code{source} or the parsed version of the file,
  according to whether \code{run} is \code{TRUE}.

  For non-R files, the value is currently just the character vector
  containing the lines of the file.  Future versions may be a little
  more clever.
}
\seealso{\code{\link{demoSource}} for a more interactive way to run a
  file of R code. }
\references{
Chambers, John M.(2008) \emph{Software for Data Analysis}, Springer.
}
\examples{
\dontrun{
runExample("madCall")  # run file "madCall.R"
runExample(53) # the example appearing on page 53 of the book.
}
}
\keyword{programming }
