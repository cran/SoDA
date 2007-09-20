\name{showLanguage}
\alias{showLanguage}
\alias{showCall}
\alias{show,language-method}
\title{Methods to show the structure of language objects  }
\description{
  Unlike the default print methods, which only deparse objects
  representing expressions in the S language, these functions, and the
  method for function \code{\link{show}} that they implement, show the
  structure of the object, to help compute with them sensibly.
}
\usage{
showLanguage(object, indent = "")
showCall(object, indent = "")
}
\arguments{
  \item{object}{ The object to be shown; usually an unevaluated
  expression in the language, but anything is legal. }
  \item{indent}{ Indentation string; incremented by four spaces for
      each recursive level of call. }
}
\details{
  Expressions in R, other than names, generally have a recursive
      call-like structure, with the first element being the function
      called and the remainder being the arguments.  The methods
      presented here display the object in this form.  See the examples.
}
\value{
\code{invisible(object)}
}

\seealso{ \code{\link{deparse}} }
\examples{
show(quote(x))

show(as.name("[["))

xx <- quote(f(1:10))

show(xx)

## a call to a function object

f <- function(x)x+1

xx[[1]] <- f

show(xx)

## a literal function expression in the call
## (note: the function definition has not yet been evaluated)

yy <- quote((function(x)x+1)(1:10))

show(yy)

}
\keyword{programming}
