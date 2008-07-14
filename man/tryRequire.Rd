\name{tryRequire}
\alias{tryRequire}
\title{Error-free test for requiring a package }
\description{
 This function reliably tries to attach a package and silently returns \code{FALSE} if the package cannot be attached.  Unlike \code{\link{require}}, it does not generate an error if the package exists but cannot be attached (e.g., because of version requirements).
}
\usage{
tryRequire(what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{The name of the package}
}\details{
The function intends to run silently, but this is not uniformly possible, since the \code{quietly = TRUE} option to \code{require()} does not suppress messages from other packages attached through dependencies in this package.

 The value is not returned invisibly, as it would be with \code{require()}.
}
\value{
 \code{TRUE} if the package was successfully attached and \code{FALSE} otherwise.
}
\examples{
tryRequire(RSPerl)
}
\keyword{programming}
