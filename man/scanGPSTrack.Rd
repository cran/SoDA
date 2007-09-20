\name{scanGPSTrack}
\alias{scanGPSTrack}
\title{Scan GPS Track Data into a GPSTrack class Object }
\description{
Attempts to scan a file or other connection containing GPS track information in a typical format, and to store the data in an object from class \code{\linkS4class{GPSTrack}}.
Optional arguments define the layout of lines and the format used for date/time information
}
\usage{
scanGPSTrack(con, fields, dateTimeFormat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{ A file name or open connection from which to read the data. }
  \item{fields}{An optional list for use by \code{\link{scan}} to interpret the fields in each line of the file.  The list must include the names in the default list, which is \code{list(date = "", time = "", lat = 0, lon = 0, el = 0)}.  Note that other fields can be included to skip over data not needed for the track information, and that the field names can be longer, provided that the versions here match by partial matching.
 }
  \item{dateTimeFormat}{An optional character string format for the date and time, in the scheme used by \code{\link{strptime}}.  The default is the standard date/time format,  \code{"\%Y-\%m-\%d\%H:\%M:\%S"}.
}
}
\details{
  Data is read from the connection by \code{scan()}, and the date and time components are then re-read by \code{strptime}, and a new object of class \code{"GPSTrack"} is created from the results.  The scan takes in all the lines available.
}
\value{
  An object of \code{\linkS4class{GPSTrack}} containing the coordinates and time as scanned.
}
\seealso{ \code{\linkS4class{GPSTrack}} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}

