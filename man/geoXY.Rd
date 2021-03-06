\name{geoXY}
\alias{geoXY}
\title{ Geodetic coordinates from latitude and longitude }
\description{
  Given a set of points on the earth's surface, in latitude and longitude form, this function returns the corresponding coordinates in \code{X} (east-west) and \code{Y} (north-south) distances along the surface of the earth, from a specified origin.
}
\usage{
geoXY(latitude, longitude, lat0, lon0, unit = 1)
}
\arguments{
  \item{latitude, longitude}{ Pairs of latitude and longitude values for the points to be used. }
  \item{lat0, lon0}{ The two latitude, longitude defining the origin for the desired coordinates.  By default, the southwest corner of the data; that is, the minimum values for the supplied latitude and longitude coordinates. }
  \item{unit}{ The unit to be used for the coordinates, in meters; e.g., \code{unit=1000} causes the coordinates to be in kilometers. }
}
\details{
The coordinates returned are an alternative to projecting the points onto a plane or other surface.
Unlike projections, there is no distortion or approximation involved, other than computational error in the algorithm for geodetic distances.
The coordinates are in principle exact replications of the latitude and longitude, but expressed in distances along the corresponding horizontal and vertical geodesics.
Essentially, the coordinates are rotated to a parallel of latitude and a north-south meridian through the \code{origin}, and distances returned along those lines to the latitude and longitude of the data points.
For  purposes of data visualization, the advantage is that the points are suitable for plotting as \code{x, y} values directly, regardless of the location, so long as the range of the latitude is not large compared to the surface of the earth.

The specific computation can be imagined as follows.  For each pair of latitude and longitude in the data, the corresponding x coordinate is the distance from the origin to a point that has the same latitude as the origin and the same longitude as the data.
The y coordinate is the distance from the origin to a point with the same longitude as the origin and the same latitude as the data.
In each case the distance is distance on the surface of the earth, as computed by the algorithm in \code{\link{geoDist}}, with a sign given by the corresponding difference in latitude (for the y coordinate) or longitude (for the x coordinate).


}
\value{
  A two-column matrix of coordinates, with column names \code{"X", "Y"}.
}
\references{ Vincenty,T. (1975). Direct and inverse solutions of geodesics
on the ellipsoid with application of nested equations. \emph{Survey
Review}, vol. 23(176):88-94.
}
\seealso{  \code{\link{geoDist}}, which computes the distances. }
\examples{
\dontshow{
load(system.file("testdata/gpsObject1.rda", package = "SoDA"))
}
xy <- geoXY(gpsObject1@latitude, gpsObject1@longitude, unit = 1000)
plot(xy[,1], xy[,2], asp = 1)
\dontshow{
rm(gpsObject1, xy)
}
}
\keyword{ math }

