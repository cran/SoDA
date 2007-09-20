\name{chunksAdd}
\alias{chunksAdd}
\alias{chunksDrop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Manage counts of text chunks }
\description{
  Perl subroutines are used to add and delete chunks of text to tables of their counts.
}
\usage{

chunksAdd(table, data, convert)

chunksDrop(table, data, convert)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{table}{A proxy reference to a Perl hash containing counts, as returned from a previous
call to \code{chunksAdd()} or \code{chunksDrop()}.  On the initial call to \code{chunksAdd()} , this argument will be omitted, and initialized as an empty table.}
  \item{data}{A vector of items to be added or dropped from the counts in the table.
Typically a character vector but any mix of scalar items can be supplied.}
  \item{convert}{ Should the result be returned as a proxy reference to the table (a Perl hash), or converted to a named vector in R?  By default, the table is converted if the \code{data} argument is omitted or of zero length. }

}
\value{
  A proxy reference to a Perl hash, if \code{convert} is \code{FALSE}; otherwise a named vector (the conversion is done by the Perl interface in package \code{RSPerl}, and will
be a named vector of counts.
}
\references{\url{http://www.omegahat.org/RSPerl/} for the \code{RSPerl} interface.}
\author{John M. Chambers <jmc@r-project.org>
}
\examples{
\dontrun{
set.seed(314)
someLetters <- sample(letters, 100, TRUE)
tbl <- chunksAdd(data = someLetters[1:50])
tbl <- chunksAdd(tbl, someLetters[51:100])
tbl <- chunksDrop(tbl, someLetters[1:10])
chunksAdd(tbl) # to convert the table
}

}
\keyword{programming }
