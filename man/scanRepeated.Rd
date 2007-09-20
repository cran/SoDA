\name{scanRepeated}
\alias{scanRepeated}
\title{Read data with multiple line patterns}
\description{
Data files may have non-identical patterns so that different lines require different \code{what=} arguments to \code{scan()}.  This function takes a list of such patterns and applies them in separate calls to produce an interwoven list of the resulting data.
}
\usage{
scanRepeated(file, what, ...)
}
\arguments{
  \item{file}{The file of input data, organized so that each line can be read by a call to  \code{scan()}}
  \item{what}{ A list, whose elements are suitable as the argument of the same name to  \code{scan()}.  The first element applies to the first line of the file, the second to the second line, etc.  Elements are recycled, so that if the pattern of the file repeats after \emph{k} lines the list only needs to be of length \emph{k}. }
  \item{\dots}{Additional arguments to be passed to  \code{scan()}, typically \code{sep=} and similar controls. }
}
\details{
The function operates by setting up a call to \code{\link{mapply}} to read each line of the file as a text connection.
For this reason, really large files will be slow.  See the examples in the book for alternatives in R or Perl.
}
\value{
  A list, with one element per line.  When the pattern of the lines repeats, this has the data form of a matrix of type \code{"list"}, with \emph{k} rows.  The list can be restructured in a variety of ways, but its format is often suitable as is for computations that iterate over the sets of \emph{k} line types, as in the book example.
}
\seealso{ \code{\link{mapply}}, \code{\link{scan}} }
\examples{
\dontrun{
 what = list(
    list(abb = "", population = 1, area = 1, 
         latitude = 1, longitude = 1),
    list(name = "", population = 1, area = 1)
)
data <- scanRepeated("stateCities.txt", what, sep = ",")}
## produces a list of 100 elements, alternately state and city data.
}
\keyword{data}

