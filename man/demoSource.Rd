\name{demoSource}
\alias{demoSource}
\alias{demoInput}
\alias{demoExample}
\title{Flexible execution of R source for demonstrations }
\description{
R expressions in a source file are shown and evaluated sequentially, using a simple prompt (in a separate R session).  Additional R expressions can be inserted at any point in the demonstration.
}
\usage{
demoSource(demo, inputCon,  where = .GlobalEnv)
demoInput(path=)

demoExample(name, package = "SoDA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{demo}{Either the file name or input connection for the R source to be demonstrated, or an object from class \code{"demoSource"}.  The latter allows resuming a demo. }
  \item{inputCon}{The connection from which to read prompt input.  Usually omitted in which case a call to \code{demoInput()} should occur in another R session in the same directory (see the details).}
  \item{where}{The environment where the demo expressions should be evaluated.  By default, in the global environment.}


  \item{path}{The file system path where user input is passed to the demo controller.  Must be a writable location and be known to both R sessions.  Usually omitted, in which case a suitable \code{fifo} is created to communicate with \code{demoSource()}.}


  \item{name}{For \code{demoExample()}, the name of the example file, with or without a \code{".R"} suffix.  There should be a corresponding file in the \code{"Examples"} subdirectory of the package.}
  \item{package}{The name of the package to use to find the example file.}
}
\details{
Calling \code{demoSource} starts up a controller that reads input from the input connection.  This is normally a file opened in the directory where both R sessions are running.  In the control session, the user calls \code{demoInput}, which then goes into a loop reading terminal input and writing to the file.

In the demo R session, the user then calls \code{demoSource}, usually just supplying the file name for the source file.
The controller now reads from a \code{\link{fifo}} connected to the file and interprets the lines as follows.  Empty lines (the usual) alternately display and evaluate single lines from the source file.
A line consisting only of a comma is a continuation:  The next line of the source is read and displayed but not yet evaluated.  Use this mechanism to collect a bunch of lines to be evaluated together, such as a function definition.  When the last line of the bunch is displayed, enter an empty line to evaluate all the lines at once.

Any other input is interpreted as a literal expression that the user wants evaluated instead of the next source line.
}
\value{
  \code{demoSource()} and \code{demoExample()} return, invisibly, the \code{"demoSource"} object describing the current state of the demo.
}
\seealso{
	\code{\link{demoSource-class}()}
}\examples{
}
\keyword{programming}
