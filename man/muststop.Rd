\name{muststop}
\alias{muststop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test for an expression that should result in an error}
\description{
 This function tests the assertion that there is an error in the expression and therefore that evaluating it will generate an error condition.
}
\usage{
muststop(expr, silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{ Any R expression }
    \item{silent}{if \code{FALSE}, the error message will be reported.}
}
\value{
  If an error is generated, the function returns the corresponding \code{\link{condition}} object; if not, then \code{muststop()} will itself generate an error.
}

\seealso{\code{\link{stopifnot}}}
\examples{
muststop(sqrt("abc"))
}
\keyword{programming }
