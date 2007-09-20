\name{randomGeneratorState-class}
\docType{class}
\alias{randomGeneratorState-class}
\alias{show,randomGeneratorState-method}

\title{Class of Objects Representing Random Generator State}
\description{A class union, initially containing only \code{"integer"}, the data type for standard R generator states.  Might eventually allow other forms of generator state, but not likely for built-in R generators. }
\section{Objects from the Class}{A Class Union, no objects.}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "randomGeneratorState")}: The method assumes the standard R state; other generator state classes would likely override this. }
	 }
}
\seealso{
    Class \code{\linkS4class{simulationResult}} has this class as slots.
}
\examples{
showClass("randomGeneratorState")
}
\keyword{classes}
