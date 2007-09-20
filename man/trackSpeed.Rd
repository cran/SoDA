\name{trackSpeed}
\alias{trackSpeed}
\title{ Compute interpoint speeds along a track }
\description{
  Given a vector or matrix of successive positions (in any number of dimensions), and the corresponding times, returns a vector of the average speed over each step in the track.
}
\usage{
trackSpeed(coords, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{Either a vector (1 dimension) or a matrix whose rows are successive points along a measured track. }
  \item{time}{ A vector of times corresponding to the positions; either numeric or one of R's standard time classes. }
}
\details{
  The function computes Euclidean distances along the path, in an arbitrary number of dimensions, though usually 1, 2, or 3, and divides by the corresponding differences in time.  Missing values are allowed in either coordinates or time, but will propagate into the computed speeds, in the sense that the speed is \code{NA} if any of the coordinates or times at either end of the interval are missing.
}
\value{
  Numeric vector of speeds.
}
\examples{
\dontshow{
load(system.file("testdata/gpsObject1.rda", package = "SoDA"))
object <- gpsObject1
}
xy <- geoXY(object@latitude, object@longitude)
trackSpeed(cbind(xy, object@elevation), object@time)
\dontshow{
rm(gpsObject1, object, xy)
}
}
\keyword{ math }
