\name{promptAll}
\alias{promptAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create an outline of documentation for multiple objects }
\description{
  Documentation in outline form will be generated to document together all the objects (typically functions) whose names are given.
}
\usage{
promptAll(objects, name, filename, where, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{objects}{ The names of the objects to be documented. }
  \item{name}{ The name for the documentation file; by default the name of the first of the objects is used.}
  \item{filename}{ The file on which to save the outline of the documentation.  By default, appends \code{".Rd"} to \code{name}. }
  \item{where}{ Optional environment where the objects will be found.  By default, uses the top level environment of the call to \code{promptAll()}.}
  \item{\dots}{ Optional arguments to be passed on to \code{\link{prompt}()}.}
}
\details{
 The function \code{\link{prompt}()} is called for each of the objects.  The \code{usage} and \code{arguments} sections of the individual documentation are merged.  In particular, all shared argument names will be listed only once in the documentation shell.
}
\value{
Nothing useful.  Called for its side effect.
}
\seealso{ \code{\link{packageAdd}} }
\examples{
\dontrun{
promptAll(objects(pattern="tri*"), "triDiagonal")
}
}
\keyword{ documentation }
\keyword{ programming }

