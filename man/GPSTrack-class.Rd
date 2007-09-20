\name{GPSTrack-class}
\docType{class}
\alias{GPSTrack-class}

\title{Class "GPSTrack"  }
\description{	Objects representing GPS Track information, with separate slots for each of the geodetic coordinates and for time.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GPSTrack", ...)}, or read from a typical file by  calling \code{scanGPSTRACK()}.
}
\section{Slots}{
	 \describe{
    \item{\code{latitude}, \code{longitude}, \code{elevation}:}{Objects of class \code{"numeric"} containing the coordinates of points on the track.  A valid object requires these to be of the same length and suitable numeric ranges. }
    \item{\code{time}:}{Object of class \code{"DateTime"}; a valid object must have the same length as the coordinates. }
  }
}


\seealso{
	 \code{\link{scanGPSTrack}} for reading in this data; 
    \code{\linkS4class{DateTime}}, the virtual class for the time information (usually \code{"POSIXct"} for the actual times.
}
\keyword{classes}
