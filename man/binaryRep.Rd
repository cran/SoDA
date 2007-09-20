\name{binaryRep}
\alias{binaryRep}
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
  See example in book.
}
\keyword{ math }

