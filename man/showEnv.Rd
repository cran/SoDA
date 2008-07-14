\name{showEnv}
\alias{showEnv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A decent method to show or print enviornments }
\description{
Interprets the \code{"name"} attribute, if any, of the environment similarly to the default print method, but spares the reader the redundant and unreadable attributes themselves.
 }
\usage{
showEnv(x)
}
\arguments{
  \item{x}{Either an environment or something \code{\link{as.environment}()} will turn into one. }
}
\value{
  the environment, invisibly.
}
\keyword{programming}
