\name{exampleFiles}
\alias{exampleFiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Path to Files in the Examples Subdirectory }
\description{
  Given one or more file names, returns the corresponding paths to files in the \code{"Examples"} subdirectory of a package.  With the \code{names} argument missing, returns the names of all such files.
}
\usage{
exampleFiles(names = character(), package = "SoDA")
}
\arguments{
  \item{names}{Names of the files.  For R source, the suffix such as \code{".R"} will be added if omitted. }
  \item{package}{The name of the package containing the \code{"Examples"} directory; by default, this package. }
}

\value{
  Path names for all the matching files, or for all such files if \code{names} was missing.
}
\seealso{  \code{\link{demoExample}} to run such files as demos. }
\author{
John M. Chambers <jmc@r-project.org>
}
\keyword{programming}


