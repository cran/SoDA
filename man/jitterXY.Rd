\name{jitterXY}
\alias{jitterXY}
\alias{jitterX}
\alias{jitterY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add random noise to data based on graphics character size }
\description{
  The input data, for \code{x} or \code{y} axis data, is jittered by a uniform random amount, scaled to the width and height of a character if this data were being plotted.
}
\usage{
jitterXY(x = c(0, 1), y = c(0, 1), xscale = 1, yscale = 1)
jitterX(x, scale = 1)
jitterY(y, scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Numeric data that might be plotted on the horizontal axis. }
  \item{y}{ Numeric data that might be plotted on the vertical axis.}
  \item{xscale, yscale, scale}{ How much to scale the random noise on the \code{x}, \code{y}  axis}
}
\value{
  For \code{jitterXY} a  list, with \code{x} and \code{y} components containing the perturbed versions of the corresponding arguments.  Giving this list to \code{\link{plot}} produces a scatter plot of the two sets of data.
 If only one coordinate is jittered a vector is returned; this is always the case with \code{jitterX} and \code{jitterY}.
}
\seealso{  \code{\link{jitter}}, which scales data without regard to the plotting parameters. }
\examples{
\dontrun{
  with(sotu, plot(jitterXY(economy, war+peace)))
}
}
\keyword{dplot}

