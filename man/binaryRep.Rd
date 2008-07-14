\name{binaryRep}
\alias{binaryRep}
\alias{binaryRep-class}
\alias{binaryRepA}
\alias{binaryRepBits}
\alias{binaryRepPowers}
\title{Generate Binary Representation in R}
\description{
 Functions to generate a binary representation from numeric data, as an example of 
vectorizing computations.
}
\usage{

binaryRep(data, m = .Machine$double.digits)

binaryRepA(m)

binaryRepBits(data)

binaryRepPowers(n, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{ numeric data }
  \item{m}{ number of bits in mantissa }



  \item{n}{ range for powers}
}
\value{
  The main function, \code{binaryRep} returns an object from class \code{"binaryRep"} providing  the components of
  the representation of \code{data}, as well as the original data
  itself.

  The other functions are helper functions; see the examples in the book.
}
\section{Class \code{binaryRep}}{
  The object returned 
	 \describe{
    \item{\code{original}:}{The original data, of class \code{"numeric"} }
    \item{\code{sign}, \code{exponent}:}{Objects of class
        \code{"integer"} for the sign and exponent. }
    \item{\code{bits}:}{Object of class \code{"raw"} for the significand. }
  }
}
\examples{
binaryRep(c(.1, .25, 1/3))
}
\keyword{ math }

