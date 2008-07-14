\name{simulationResult}
\alias{resetSeed}
\alias{simulationResult}
\alias{simulationResult-class}
\alias{show,simulationResult-method}

\title{ Create a simulation result, including first and last state}
\description{
  The class \code{"simulationResult"} and the function of the same name record an arbitrary simulation result, along with the state of the random number generators at the start and at the end of the simulation, allowing trustworthy verification or repetition of the simulation.
}
\usage{

simulationResult(value, seed)

resetSeed(object, last = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{value}{The simulation to be done and saved. }
  \item{seed}{ Optionally, the argument to \code{\link{set.seed}}(), to set the first state of the generator.  If omitted, the generator should have been initialized before the call, and the current value of \code{.Random.seed} will be used.}

  \item{object}{ An object from the \code{"simulationResult"} class, usually from a call to the \code{simulationResult()} function. }
  \item{last}{ Optional flag, if \code{TRUE}, then the generator is reset from the last state of \code{object}, otherwise from the first. }
}

\value{
  Function \code{simulationResult} returns an object of that class, with the \code{expr} and \code{result} slots set to the unevaluated and evaluated version of the \code{value} argument.
}
\section{Slots}{
	 \describe{
    \item{\code{firstState}, \code{lastState}:}{Objects of class \code{"randomGeneratorState"}, containing the state of the generator before and after the simulation. }
    \item{\code{expr}:}{The expression evaluated to produce the simulation. }
    \item{\code{result}:}{The object returned by the simulation. }
  }
}
\seealso{
  \code{\link{randomSlippage}}
}
\keyword{ programming}
\keyword{classes}

