\name{geoDist}
\alias{geoDist}
\alias{geodetic}

\title{ Geodetic distances from latitude and longitude }
\description{
 Given two sets of points on the earth's surface in latitude, longitude form, returns the geodetic distances in meters between corresponding points.
}
\usage{
geoDist(lat1, lon1, lat2, lon2, NAOK = TRUE, DUP = TRUE)
}

\arguments{
  \item{lat1, lon1, lat2, lon2}{ Latitude and longitude co-ordinates for the two sets of points. }
  \item{NAOK}{ Are \code{NA} values allowed in the co-ordinates?  Default \code{TRUE}. If so, corresponding elements of the distance will also be \code{NA}.}
  \item{DUP}{ Value for the  \code{DUP} argument to \code{.Fortran()}. }
}
\details{
  Uses a classic Fortran algorithm implementing a method that allows for the non-spherical shape of the earth.  See comments in the Fortran code for the history of the implementation.
}
\value{
  numeric vector of distances, optionally including \code{NA} values if those are allowed and present in any of the coordinates.
}
\references{ 
Vincenty,T. (1975). Direct and inverse solutions of geodesics
on the ellipsoid with application of nested equations. \emph{Survey
Review}, vol. 23(176):88-94.
}
\seealso{ For the \code{DUP} argument, see  \code{\link{.Fortran}}. }
\examples{
}
\keyword{ math }% at least one, from doc/KEYWORDS
