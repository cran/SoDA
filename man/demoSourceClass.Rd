\name{demoSource-class}
\docType{class}
\alias{demoSource-class}

\title{Class "demoSource" for objects used with demoSource() function }
\description{Objects from this class are created and manipulated during interactive sessions with the \code{\link{demoSource}()} function.  They  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("demoSource", ...)}.
Generally, objects are created by \code{\link{demoSource}()} and
modified by the various utility functions that process control and
demo input. Objects from the class are normally used to save and
restart an incomplete demo.
}
\section{Slots}{
	 \describe{
    \item{\code{lines}:}{Object of class \code{"character"} holding the lines of the source connection. }
    \item{\code{pos}:}{Current position in the source buffer (the number of lines that have been processed so far). }
    \item{\code{partial}:}{While an expression is being parsed, lines in the current expression are copied here. }
    \item{\code{expr}:}{When an expression has been parsed, the parsed expression is stored here. }
    \item{\code{value}:}{When an expression has been evaluated, the value is stored here. }
    \item{\code{envir}:}{Object of class \code{"environment"}, the environment in which expressions have been evaluated.}
    \item{\code{state}:}{Character string used to indicate the current state of the demo.  States \code{"partial"}, \code{"parsed"}, and \code{"evaluated"} correspond having read some (or all) of the current input, to having parsed that (and determined that it's a complete expression), and having evaluated the expression.

See \code{\link{demoSource}()} for how one gets to these states; for example, the typical \code{""} control input first reads and parses to a complete expression, and then on the next such input evaluates.

The various utility functions used by  \code{\link{demoSource}()} modify the demo object they receive to reflect the step(s) taken, and then return the modified object.  The call to \code{\link{demoSource}()} itself returns (invisibly) the demo object as it stands when the demo exits.  Using the returned object, one can pause and resume a demo or interleave multiple demos.}
  }
}

\seealso{
	\code{\link{demoSource}()}
}
\keyword{classes}
