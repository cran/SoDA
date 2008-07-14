\name{triDiagonal}
\alias{triDiag}
\alias{triDiag2}
\alias{triDiag2S}
\alias{triDiag3}
\alias{triDiag3S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tridiagonal Matrix Functions }
\description{
  Functions to create forms of tridiagonal matrix objects.
}
\usage{

triDiag(diagonal, upper, lower, nrow = length(diagonal), ncol = nrow)

triDiag2(diagonal, upper, lower, nrow = length(diagonal), ncol = nrow)

triDiag2S(diagonal, upper, lower, r = length(diagonal))

triDiag3(diagonal, upper, lower, nrow = length(diagonal), ncol = nrow)

triDiag3S(diagonal, upper, lower, r = length(diagonal))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{diagonal, upper, lower}{Numeric data to store in these positions. }
  \item{nrow, ncol}{Number of rows and columns }


  \item{r}{ size of square matrix }


}
\value{
  Tridiagonal matrices, constructed by a variety of computations, as described in the book.
}
\keyword{array}
